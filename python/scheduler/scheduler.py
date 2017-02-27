#!/usr/local/bin/python3

# Todo:
# add routine parsing
# add scheduler
# commit! make metadata a token and correct grammar?

import collections
import copy
import datetime
import json
import os
import pprint
import re
import resource
import string
import sys
from itertools import cycle, islice

# TOKENS:
#   GOALS    "* GOALS "
#   ROUTINE  "* ROUTINE "
#   LEVEL1   "* "
#   LEVEL2   "** "
#   LEVEL3   "*** "
#   LEVEL4   "**** "
#   TASK     "-"
#   SUBTASK  "  -"
#   NEWLINE  "\n"
#   ACTIVE   "+" | "-"
#   COMPLETE "[ ]" | "[?]"
#   METADATA
#   TEXT

# GRAMMAR:
#   _Start           : _GoalsEntry _RoutineEntry
#   _GoalsEntry      : <LEVEL1> <GOALS> <NEWLINE> _Goals
#   _Goals           : _Goal
#                    | _Goal _Goals
#   _Goal            : <LEVEL2> <ACTIVE> <TEXT> <NEWLINE> _Projects
#   _Projects        : _Project
#                    | _Project Projects
#   _Project         : <LEVEL3> <ACTIVE> <TEXT> <NEWLINE>
#                    | <LEVEL3> <ACTIVE> <TEXT> <NEWLINE> _ProjectChildren
#   _ProjectChildren : _SubProjects
#                    | _Tasks
#   _Subprojects     : _Subproject
#                    | _Subproject _Subprojects
#   _Subproject      : <LEVEL4> <ACTIVE> <TEXT> <NEWLINE>
#                    | <LEVEL4> <ACTIVE> <TEXT> <NEWLINE> _Tasks
#   _Tasks           : _Task
#                    | _Tasks
#   _Task            : <TASK> <COMPLETE> <TEXT> <NEWLINE>
#                    | <TASK> <COMPLETE> <TEXT> <NEWLINE> _Subtasks
#   _Subtasks        : _Subtask
#                    | _Subtasks
#   _Subtask         : <SUBTASK> <COMPLETE> <TEXT> <NEWLINE>
#   _RoutineEntry    : <LEVEL1> <ROUTINE> <NEWLINE> _Week
#   _Week            : _Day _Day _Day _Day _Day _Day _Day
#   _Day             : <LEVEL2> <TEXT> <NEWLINE> _Slots
#   _Slots           : _Slot
#                    : _Slot _Slots
#   _Slot            : <LEVEL3> <TIME> <POMODOROS> <TEXT> <NEWLINE>


def jdefault(object):
    return object.__dict__


def maybe_to_int(string):
    try:
        return int(string)
    except ValueError:
        return string


class Todo:

    def __init__(self, goals=[], routine=[]):
        self.goals = goals
        self.routine = routine

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Goal:

    def __init__(self, name, active, projects=[], metadata={}):
        self.name = name
        self.active = active
        self.projects = projects
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Project:

    def __init__(self, name, active, subprojects=[], tasks=[], metadata={}):
        self.name = name
        self.active = active
        self.subprojects = subprojects
        self.tasks = tasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Subproject:

    def __init__(self, name, active, tasks=[], metadata={}):
        self.name = name
        self.active = active
        self.tasks = tasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Task:

    def __init__(self, name, complete, subtasks=[], metadata={}):
        self.name = name
        self.complete = complete
        self.subtasks = subtasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Subtask:

    def __init__(self, name, complete, metadata={}):
        self.name = name
        self.complete = complete
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class LexerException(Exception):
    def __init__(self, message="", line=0, col=0):
        self.line = line
        self.col = col
        self.message = "Lexer Error {}:{} :: {}".format(line, col, message)


class ParserException(Exception):
    def __init__(self, message="Syntax error.", token=None):
        if token:
            self.token = token
            self.line = token.line
            self.col = token.col
        else:
            self.line = 0
            self.col = 0
        fmt = "Parser Error {}:{} :: {}"
        self.message = fmt.format(self.line, self.col, message)

    def __str__(self):
        return self.message


class Token:
    GOALS = "GOALS"
    ROUTINE = "ROUTINE"
    LEVEL1 = "LEVEL1"
    LEVEL2 = "LEVEL2"
    LEVEL3 = "LEVEL3"
    LEVEL4 = "LEVEL4"
    TASK = "TASK"
    SUBTASK = "SUBTASK"
    ACTIVE = "ACTIVE"
    COMPLETE = "COMPLETE"
    TEXT = "TEXT"
    METADATA = "METADATA"
    NEWLINE = "NEWLINE"

    TYPES = [
        GOALS,
        ROUTINE,
        LEVEL1,
        LEVEL2,
        LEVEL3,
        LEVEL4,
        TASK,
        SUBTASK,
        ACTIVE,
        COMPLETE,
        TEXT,
        METADATA,
        NEWLINE
        ]

    LEVEL1_LEXEME = "* "
    LEVEL2_LEXEME = "** "
    LEVEL3_LEXEME = "*** "
    LEVEL4_LEXEME = "**** "
    GOALS_LEXEME = "GOALS"
    ROUTINE_LEXEME = "ROUTINE"
    TASK_LEXEME = "- "
    SUBTASK_LEXEME = "  - "

    GOALS_RE = re.compile("(\*) (GOALS)(\n)")
    ROUTINE_RE = re.compile("(\*) (ROUTINE)(\n)")
    GOAL_RE = re.compile("(\*{2}) ([+\-]) (.*)(\n)")
    PROJECT_RE = re.compile("(\*{3}) ([+\-]) ([^\[\]]*) ?(\[.*\])?(\n)")
    SUBPROJECT_RE = re.compile("(\*{4}) ([+\-]) ([^\[\]]*) ?(\[.*\])?(\n)")
    TASK_RE = re.compile("(\-) (\[.\]) ([^\[\]]*) ?(\[.*\])?(\n)")
    SUBTASK_RE = re.compile("(\ \ \-) (\[.\]) ([^\[\]]*) ?(\[.*\])?(\n)")

    def __init__(self, type="UNKNOWN", line=0, col=0, lexeme="", md={}):
        if type not in Token.TYPES:
            raise LexerException("Unknown token type: {}".format(type), None)
        else:
            self.type = type
            self.line = line
            self.col = col
            self.lexeme = lexeme
            self.metadata = md

    def __repr__(self):
        fmt = "Token(type='{}',line={},col={},lexeme='{}',metadata={})"
        type = self.type
        line = self.line
        col = self.col
        lex = self.lexeme
        md = self.metadata
        return fmt.format(type, line, col, lex, md)

    # def __str__(self):
    #     return "{}:{}:{}".format(self.line, self.col, self.type)

    def is_of_type(self, type):
        if type not in Token.TYPES:
            raise LexerException("Unknown token type: {}".format(type), None)
        return type == self.type

    def active(self):
        if '+' == self.lexeme:
            return True
        elif '-' == self.lexeme:
            return False
        else:
            return None

    def complete(self):
        if '[ ]' == self.lexeme:
            return False
        else:
            return True


class OrgLexer:

    def __init__(self, filename):
        self.filename = filename

    def tokenize(self):
        tokens = collections.deque()
        with open(self.filename, 'r') as todofile:
            lines = todofile.readlines()
            line_no = 1
            for line in lines:
                line_tokens = self._tokenize_line(line_no, line)
                tokens.extend(line_tokens)
                line_no += 1
        return tokens

    def _tokenize_line_level1(self, line_no, line):
        line_rest = line[len(Token.LEVEL1_LEXEME):]
        if line_rest.startswith(Token.GOALS_LEXEME):
            m = Token.GOALS_RE.match(line)
            if m:
                return [
                    Token(Token.LEVEL1, line_no, m.start(1), m.group(1)),
                    Token(Token.GOALS, line_no, m.start(2), m.group(2)),
                    Token(Token.NEWLINE, line_no)
                    ]
            else:
                raise LexerException("Bad top-level goals entry.", line_no)
        elif line_rest.startswith(Token.ROUTINE_LEXEME):
            m = Token.ROUTINE_RE.match(line)
            if m:
                return [
                    Token(Token.LEVEL1, line_no, m.start(1), m.group(1)),
                    Token(Token.ROUTINE, line_no, m.start(2), m.group(2)),
                    Token(Token.NEWLINE, line_no)
                    ]
            else:
                raise LexerException("Bad top-level routine entry.", line_no)
        else:
            raise LexerException("Bad entry.", line_no)

    def _tokenize_line_level2(self, line_no, line):
        m = Token.GOAL_RE.match(line)
        if m:
            return [
                Token(Token.LEVEL2, line_no, m.start(1), m.group(1)),
                Token(Token.ACTIVE, line_no, m.start(2), m.group(2)),
                Token(Token.TEXT, line_no, m.start(3), m.group(3)),
                Token(Token.NEWLINE, line_no, m.start(4))
                ]
        else:
            raise LexerException("Bad goal entry.", line_no)

    def _tokenize_line_level3(self, line_no, line):
        m = Token.PROJECT_RE.match(line)
        if m:
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            return [
                Token(Token.LEVEL3, line_no, m.start(1), m.group(1)),
                Token(Token.ACTIVE, line_no, m.start(2), m.group(2)),
                Token(Token.TEXT, line_no, m.start(3), m.group(3), metadata),
                Token(Token.NEWLINE, line_no, m.start(5))
                ]
        else:
            fmt = "Bad project entry: '{}'"
            raise LexerException(fmt.format(line), line_no)

    def _tokenize_line_level4(self, line_no, line):
        m = Token.SUBPROJECT_RE.match(line)
        if m:
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            return [
                Token(Token.LEVEL4, line_no, m.start(1), m.group(1)),
                Token(Token.ACTIVE, line_no, m.start(2), m.group(2)),
                Token(Token.TEXT, line_no, m.start(3), m.group(3), metadata),
                Token(Token.NEWLINE, line_no, m.start(5))
                ]
        else:
            raise LexerException("Bad subproject entry.", line_no)

    def _tokenize_line_task(self, line_no, line):
        m = Token.TASK_RE.match(line)
        if m:
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            return [
                Token(Token.TASK, line_no, m.start(1), m.group(1)),
                Token(Token.COMPLETE, line_no, m.start(2), m.group(2)),
                Token(Token.TEXT, line_no, m.start(3), m.group(3), metadata),
                Token(Token.NEWLINE, line_no, m.start(5))
            ]
        else:
            raise LexerException("Bad task entry.", line_no)

    def _tokenize_line_subtask(self, line_no, line):
        m = Token.SUBTASK_RE.match(line)
        if m:
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            return [
                Token(Token.SUBTASK, line_no, m.start(1), m.group(1)),
                Token(Token.COMPLETE, line_no, m.start(2), m.group(2)),
                Token(Token.TEXT, line_no, m.start(3), m.group(3), metadata),
                Token(Token.NEWLINE, line_no, m.start(5))
            ]
        else:
            raise LexerException("Bad subtask entry.", line_no)

    def _tokenize_line(self, line_no, line):
        if line.startswith(Token.LEVEL1_LEXEME):
            return self._tokenize_line_level1(line_no, line)
        elif line.startswith(Token.LEVEL2_LEXEME):
            return self._tokenize_line_level2(line_no, line)
        elif line.startswith(Token.LEVEL3_LEXEME):
            return self._tokenize_line_level3(line_no, line)
        elif line.startswith(Token.LEVEL4_LEXEME):
            return self._tokenize_line_level4(line_no, line)
        elif line.startswith(Token.TASK_LEXEME):
            return self._tokenize_line_task(line_no, line)
        elif line.startswith(Token.SUBTASK_LEXEME):
            return self._tokenize_line_subtask(line_no, line)
        else:
            fmt = "Bad entry: {}:{}"
            raise LexerException(fmt.format(line_no, line), line_no)

    def _tokenize_metadata(self, line_no, col_no, metadata):
        try:
            if metadata:
                d = {y[0]: maybe_to_int(y[1])
                     for y
                     in [x.split(':') for x in metadata[1:-1].split(',')]
                     }
                return d
            else:
                return {}
        except Exception:
            fmt = "Bad metadata entry: {}"
            raise LexerException(fmt.format(metadata), line_no, col_no)


class OrgParser:

    def __init__(self, tokens):
        self.tokens = tokens
        self.ast = []

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def parse(self):
        self.ast = self._Start()
        return self.ast

    def _check_token_stream_length(self, minimum_length):
        if minimum_length > len(self.tokens):
            last_token = self.tokens[-1:]
            msg = "Unexpected end of token stream."
            raise ParserException(msg, last_token)

    def _Start(self):
        """
        _Start : _GoalsEntry _RoutineEntry
        """
        # print("XXX _Start")
        goals = self._GoalsEntry()
        # routine = self._Routine()
        routine = None
        return Todo(goals, routine)

    def _GoalsEntry(self):
        """
        _GoalsEntry : <LEVEL1> <NEWLINE> _Goals
        """
        # print("XXX _GoalsEntry")
        self._check_token_stream_length(3)
        level1_token = self.tokens.popleft()
        GOALS_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not level1_token.is_of_type(Token.LEVEL1):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL1_LEXEME, level1_token.lexeme)
            raise ParserException(msg, level1_token)
        if not GOALS_token.is_of_type(Token.GOALS):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.GOALS_LEXEME, GOALS_token.lexeme)
            raise ParserException(msg, GOALS_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        return self._Goals()

    def _Goals(self):
        """
        _Goals : _Goal
               | _Goal _Goals
        """
        # print("XXX _Goals")
        goal = self._Goal()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL2):
            goals = self._Goals()
            return [goal] + goals
        else:
            return [goal]

    def _Goal(self):
        """
        _Goal : <LEVEL2> <ACTIVE> <TEXT> <NEWLINE> _Projects
        """
        # print("XXX _Goal")
        self._check_token_stream_length(4)

        level2_token = self.tokens.popleft()
        active_token = self.tokens.popleft()
        text_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not level2_token.is_of_type(Token.LEVEL2):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL2_LEXEME, level2_token.lexeme)
            raise ParserException(msg, level2_token)
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected goal active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected goal name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        name = text_token.lexeme
        active = active_token.active()
        projects = self._Projects()

        return Goal(name, active, projects)

    def _Projects(self):
        """
        _Projects : _Project
                  | _Project Projects
        """
        # print("XXX _Projects")
        project = self._Project()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL3):
            projects = self._Projects()
            return [project] + projects
        else:
            return [project]

    def _Project(self):
        """
        _Project : <LEVEL3> <ACTIVE> <TEXT> <NEWLINE>
                 | <LEVEL3> <ACTIVE> <TEXT> <NEWLINE> _ProjectChildren
        """
        # print("XXX _Project")
        self._check_token_stream_length(4)

        level3_token = self.tokens.popleft()
        active_token = self.tokens.popleft()
        text_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not level3_token.is_of_type(Token.LEVEL3):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL3_LEXEME, level3_token.lexeme)
            raise ParserException(msg, level3_token)
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected project active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected project name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        name = text_token.lexeme
        active = active_token.active()
        subprojects = []
        tasks = []
        children = self.tokens and (self.tokens[0].is_of_type(Token.LEVEL4)
                                    or self.tokens[0].is_of_type(Token.TASK))
        if children:
            subprojects, tasks = self._ProjectChildren()

        metadata = text_token.metadata
        return Project(name, active, subprojects, tasks, metadata)

    def _ProjectChildren(self):
        """
        _ProjectChildren : _SubProjects
                         | _Tasks
        """
        # print("XXX _ProjectChildren")
        subprojects = []
        tasks = []
        if self.tokens[0].is_of_type(Token.LEVEL4):
            subprojects = self._Subprojects()
        elif self.tokens[0].is_of_type(Token.TASK):
            tasks = self._Tasks()
        else:
            fmt = "Expected a subproject or task, found '{}'"
            msg = fmt.format(self.tokens[0].lexeme)
            raise ParserException(msg, self.tokens[0])
        return (subprojects, tasks)

    def _Subprojects(self):
        """
        _Subprojects : _Subproject
                     | _Subproject _Subprojects
        """
        # print("XXX _Subprojects")
        subproject = self._Subproject()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL4):
            subprojects = self._Subprojects()
            return [subproject] + subprojects
        else:
            return [subproject]

    def _Subproject(self):
        """
        _Subproject : <LEVEL4> <ACTIVE> <TEXT> <NEWLINE>
                    | <LEVEL4> <ACTIVE> <TEXT> <NEWLINE> _Tasks
        """
        # print("XXX _Subproject")
        self._check_token_stream_length(4)

        level4_token = self.tokens.popleft()
        active_token = self.tokens.popleft()
        text_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not level4_token.is_of_type(Token.LEVEL4):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL4_LEXEME, level4_token.lexeme)
            raise ParserException(msg, level4_token)
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected subproject active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected subproject name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        name = text_token.lexeme
        active = active_token.active()
        tasks = []
        if self.tokens and self.tokens[0].is_of_type(Token.TASK):
            tasks = self._Tasks()

        metadata = text_token.metadata
        return Subproject(name, active, tasks, metadata)

    def _Tasks(self):
        """
        _Tasks : _Task
               | _Tasks
        """
        # print("XXX _Tasks")
        task = self._Task()
        if self.tokens and self.tokens[0].is_of_type(Token.TASK):
            tasks = self._Tasks()
            return [task] + tasks
        else:
            return [task]

    def _Task(self):
        """
        _Task : <TASK> <COMPLETE> <TEXT> <NEWLINE>
              | <TASK> <COMPLETE> <TEXT> <NEWLINE> _Subtasks
        """
        # print("XXX _Task")
        self._check_token_stream_length(4)

        TASK_token = self.tokens.popleft()
        complete_token = self.tokens.popleft()
        text_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not TASK_token.is_of_type(Token.TASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.TASK_LEXEME, TASK_token.lexeme)
            raise ParserException(msg, TASK_token)
        if not complete_token.is_of_type(Token.COMPLETE):
            fmt = "Expected task completion indicator, found '{}'."
            msg = fmt.format(complete_token.lexeme)
            raise ParserException(msg, complete_token)
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected task name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        name = text_token.lexeme
        complete = complete_token.complete()
        subtasks = []
        if self.tokens and self.tokens[0].is_of_type(Token.SUBTASK):
            subtasks = self._Subtasks()

        metadata = text_token.metadata
        return Task(name, complete, subtasks, metadata)

    def _Subtasks(self):
        """
        _Subtasks : _Subtask
                  | _Subtasks
        """
        # print("XXX _Subtasks")
        subtask = self._Subtask()
        if self.tokens and self.tokens[0].is_of_type(Token.SUBTASK):
            subtasks = self._Subtasks()
            return [subtask] + subtasks
        else:
            return [subtask]

    def _Subtask(self):
        """
        _Subtask : <SUBTASK> <COMPLETE> <TEXT> <NEWLINE>
        """
        # print("XXX _Subtask")
        self._check_token_stream_length(4)

        subtask_token = self.tokens.popleft()
        complete_token = self.tokens.popleft()
        text_token = self.tokens.popleft()
        newline_token = self.tokens.popleft()

        if not subtask_token.is_of_type(Token.SUBTASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.SUBTASK_LEXEME, subtask_token.lexeme)
            raise ParserException(msg, subtask_token)
        if not complete_token.is_of_type(Token.COMPLETE):
            fmt = "Expected subtask completion indicator, found '{}'."
            msg = fmt.format(complete_token.lexeme)
            raise ParserException(msg, complete_token)
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected subtask name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        name = text_token.lexeme
        complete = complete_token.complete()
        metadata = text_token.metadata
        return Subtask(name, complete, metadata)


class Schedule:

    def __init__(self, todo):
        pass

    def __repr__(self):
        return "SCHEDULE:\n"


def main():
    sys.setrecursionlimit(2000)

    filename = sys.argv[1]
    try:
        lexer = OrgLexer(filename)
        tokens = lexer.tokenize()
        # [print(t) for t in tokens]

        parser = OrgParser(tokens)
        todo = parser.parse()
        print(todo)

        schedule = Schedule(todo)
        # print(schedule)

    except LexerException as e:
        print(e.message)
        exit(1)
    except ParserException as e:
        print(e.message)
        exit(1)


if __name__ == "__main__":
    main()
