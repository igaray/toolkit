#!/usr/local/bin/python3

# Todo:
# add scheduler

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

# TODO / GOALS / PROJECTS / SUBPROJECTS / TASKS / SUBTASKS
# ROUTINE / DAY / SLOT / SCHEDULE

# TOKENS:
#   GOALS       "* GOALS "
#   ROUTINE     "* ROUTINE "
#   LEVEL1      "* "
#   LEVEL2      "** "
#   LEVEL3      "*** "
#   LEVEL4      "**** "
#   TASK        "-"
#   SUBTASK     "  -"
#   NEWLINE     '\n'
#   ACTIVE      '[+\-]'
#   COMPLETE    ''
#   METADATA    ''
#   TEXT        ''
#   TIME        'dd\:dd'
#   POMODOROS   'd+'

# GRAMMAR:
#   _Start           : _GoalsEntry _RoutineEntry
#   _GoalsEntry      : LEVEL1 GOALS NEWLINE _Goals
#   _Goals           : _Goal
#                    | _Goal _Goals
#   _Goal            : LEVEL2 ACTIVE TEXT NEWLINE _Projects
#   _Projects        : _Project
#                    | _Project Projects
#   _Project         : LEVEL3 ACTIVE TEXT NEWLINE
#                    | LEVEL3 ACTIVE TEXT NEWLINE _ProjectChildren
#                    | LEVEL3 ACTIVE TEXT METADATA NEWLINE
#                    | LEVEL3 ACTIVE TEXT METADATA NEWLINE _ProjectChildren
#   _ProjectChildren : _SubProjects
#                    | _Tasks
#   _Subprojects     : _Subproject
#                    | _Subproject _Subprojects
#   _Subproject      : LEVEL4 ACTIVE TEXT NEWLINE
#                    | LEVEL4 ACTIVE TEXT NEWLINE _Tasks
#                    | LEVEL4 ACTIVE TEXT METADATA NEWLINE
#                    | LEVEL4 ACTIVE TEXT METADATA NEWLINE _Tasks
#   _Tasks           : _Task
#                    | _Tasks
#   _Task            : TASK COMPLETE TEXT NEWLINE
#                    | TASK COMPLETE TEXT NEWLINE _Subtasks
#                    | TASK COMPLETE TEXT METADATA NEWLINE
#                    | TASK COMPLETE TEXT METADATA NEWLINE _Subtasks
#   _Subtasks        : _Subtask
#                    | _Subtasks
#   _Subtask         : SUBTASK COMPLETE TEXT NEWLINE
#                    | SUBTASK COMPLETE TEXT METADATA NEWLINE
#   _RoutineEntry    : LEVEL1 ROUTINE NEWLINE _Week
#   _Week            : _Day _Day _Day _Day _Day _Day _Day
#   _Day             : LEVEL2 TEXT NEWLINE _Slots
#   _Slots           : _Slot
#                    : _Slot _Slots
#   _Slot            : LEVEL3 TIME POMODOROS TEXT NEWLINE


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

    def __str__(self):
        return "* GOALS:\n" + "\n".join([str(g) for g in self.goals])

    def get_active(self):
        active_goals = [g.get_active() for g in self.goals if g.active]
        return Todo(active_goals, self.routine)


class Goal:

    def __init__(self, name, active, projects=[], metadata={}):
        self.name = name
        self.active = active
        self.projects = projects
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        if self.projects:
            projects_str = "\n".join([str(p) for p in self.projects])
            return "** " + self.name + "\n" + projects_str
        else:
            return "** " + self.name + "\n"

    def get_active(self):
        active_projects = [p.get_active() for p in self.projects if p.active]
        return Goal(self.name, self.active, active_projects, self.metadata)


class Project:

    def __init__(self, name, active, subprojects=[], tasks=[], metadata={}):
        self.name = name
        self.active = active
        self.subprojects = subprojects
        self.tasks = tasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        if self.subprojects:
            subprojects_str = "\n".join([str(sp) for sp in self.subprojects])
            return "*** " + self.name + "\n" + subprojects_str
        elif self.tasks:
            tasks_str = "\n".join([str(t) for t in self.tasks])
            return "*** " + self.name + "\n" + tasks_str
        else:
            return "*** " + self.name

    def get_active(self):
        asp = [sp for sp in self.subprojects if sp.active]
        return Project(self.name, self.active, asp, self.tasks, self.metadata)


class Subproject:

    def __init__(self, name, active, tasks=[], metadata={}):
        self.name = name
        self.active = active
        self.tasks = tasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        if self.tasks:
            tasks_str = "\n".join([str(t) for t in self.tasks])
            return "**** " + self.name + "\n" + tasks_str
        else:
            return "**** " + self.name


class Task:

    def __init__(self, name, complete, subtasks=[], metadata={}):
        self.name = name
        self.complete = complete
        self.subtasks = subtasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        if self.subtasks:
            subtasks_str = "\n".join([str(st) for st in self.subtasks])
            return "- " + self.name + "\n" + subtasks_str
        else:
            return "- " + self.name


class Subtask:

    def __init__(self, name, complete, metadata={}):
        self.name = name
        self.complete = complete
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        return "  - " + self.name


class Routine:

    def __init__(self, days):
        self.days = days

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def shift(self):
        shifts = datetime.datetime.isoweekday(datetime.datetime.today()) - 1
        for i in range(shifts):
            self.days = self.days[1:] + self.days[:1]


class Day:

    def __init__(self, name, slots):
        self.name = name
        self.slots = slots

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Slot:

    def __init__(self, time, pomodoros, todo):
        self.time = time
        self.pomodoros = pomodoros
        self.todo = todo
        # {"start": start, "category": category}

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
    TIME = "TIME"
    POMODOROS = "POMODOROS"
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
        TIME,
        POMODOROS,
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
    DAY_RE = re.compile("(\*{2}) (.*)(\n)")
    SLOT_RE = re.compile("(\*{3}) (\d\d:\d\d) (\d+) ([^\[\]]*)(\n)")

    def __init__(self, type="UNKNOWN", line=0, col=0, lexeme=""):
        if type not in Token.TYPES:
            fmt = "Unknown token type: {}"
            msg = fmt.format(type)
            raise LexerException(msg, line, col)
        else:
            self.type = type
            self.line = line
            self.col = col
            self.lexeme = lexeme
            self.value = None

            if Token.TEXT == type:
                self.value = self.lexeme
            elif Token.ACTIVE == type:
                if '+' == lexeme:
                    self.value = True
                if '-' == lexeme:
                    self.value = False
            elif Token.COMPLETE == type:
                if '[ ]' == lexeme:
                    self.value = False
                else:
                    self.value = True
            elif Token.METADATA == type:
                try:
                    d = {y[0]: maybe_to_int(y[1])
                         for y
                         in [x.split(':') for x in lexeme[1:-1].split(',')]
                         }
                    self.value = d
                except Exception:
                    fmt = "Bad metadata entry: {}"
                    raise LexerException(fmt.format(lexeme), line, col)
            elif Token.TIME == type:
                try:
                    hm = self.lexeme.split(':')
                    h = int(hm[0])
                    m = int(hm[1])
                    self.value = (h, m)
                except ValueError:
                    fmt = "Bad time format: '{}'"
                    raise LexerException(fmt.format(self.lexeme))
            elif Token.POMODOROS == type:
                try:
                    self.value = int(self.lexeme)
                except ValueError:
                    fmt = "Bad integer: '{}'"
                    raise LexerException(fmt.format(line), line, col)

    def __repr__(self):
        fmt = "Token(type='{}',line={},col={},lexeme='{}',value={})"
        type = self.type
        line = self.line
        col = self.col
        lexeme = self.lexeme
        val = self.value
        return fmt.format(type, line, col, lexeme, val)

    def __str__(self):
        if '\n' in self.lexeme:
            self.lexeme = self.lexeme.replace('\n', '')
        fmt = "{}:{}:{}({})"
        return fmt.format(self.line, self.col, self.type, self.lexeme)

    def is_of_type(self, type):
        if type not in Token.TYPES:
            raise LexerException("Unknown token type: {}".format(type), None)
        return type == self.type


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
                level1 = Token(Token.LEVEL1, line_no, m.start(1), m.group(1))
                goals = Token(Token.GOALS, line_no, m.start(2), m.group(2))
                newline = Token(Token.NEWLINE, line_no)
                return [level1, goals, newline]
            else:
                raise LexerException("Bad top-level goals entry.", line_no)
        elif line_rest.startswith(Token.ROUTINE_LEXEME):
            m = Token.ROUTINE_RE.match(line)
            if m:
                level1 = Token(Token.LEVEL1, line_no, m.start(1), m.group(1))
                routine = Token(Token.ROUTINE, line_no, m.start(2), m.group(2))
                newline = Token(Token.NEWLINE, line_no)
                return [level1, routine, newline]
            else:
                raise LexerException("Bad top-level routine entry.", line_no)
        else:
            raise LexerException("Bad entry.", line_no)

    def _tokenize_line_level2(self, line_no, line):
        m1 = Token.GOAL_RE.match(line)
        m2 = Token.DAY_RE.match(line)
        if m1:  # Goal entry
            level2 = Token(Token.LEVEL2, line_no, m1.start(1), m1.group(1))
            active = Token(Token.ACTIVE, line_no, m1.start(2), m1.group(2))
            text = Token(Token.TEXT, line_no, m1.start(3), m1.group(3))
            newline = Token(Token.NEWLINE, line_no, m1.start(4))
            return [level2, active, text, newline]
        elif m2:  # Routine day entry
            level2 = Token(Token.LEVEL2, line_no, m2.start(1), m2.group(1))
            day = Token(Token.TEXT, line_no, m2.start(2), m2.group(2))
            newline = Token(Token.NEWLINE, line_no, m2.start(3))
            return [level2, day, newline]
        else:
            fmt = "Bad goal/routine day entry: {}"
            raise LexerException(fmt.format(line), line_no)

    def _tokenize_line_level3(self, line_no, line):
        m1 = Token.PROJECT_RE.match(line)
        m2 = Token.SLOT_RE.match(line)
        if m1:  # Project entry
            level3 = Token(Token.LEVEL3, line_no, m1.start(1), m1.group(1))
            active = Token(Token.ACTIVE, line_no, m1.start(2), m1.group(2))
            text = Token(Token.TEXT, line_no, m1.start(3), m1.group(3))
            metadata = self._tokenize_metadata(line_no, m1.start(4), m1.group(4))
            newline = Token(Token.NEWLINE, line_no, m1.start(5))
            return [level3, active, text] + metadata + [newline]
        elif m2:  # Routine slot entry
            level3 = Token(Token.LEVEL3, line_no, m2.start(1), m2.group(1))
            time = Token(Token.TIME, line_no, m2.start(2), m2.group(2))
            pomodoros = Token(Token.POMODOROS, line_no, m2.start(3), m2.group(3))
            todo = Token(Token.TEXT, line_no, m2.start(4), m2.group(4))
            newline = Token(Token.NEWLINE, line_no, m2.start(5))
            return [level3, time, pomodoros, todo, newline]
        else:
            fmt = "Bad project/routine slot entry: '{}'"
            raise LexerException(fmt.format(line), line_no)

    def _tokenize_line_level4(self, line_no, line):
        m = Token.SUBPROJECT_RE.match(line)
        if m:
            level4 = Token(Token.LEVEL4, line_no, m.start(1), m.group(1))
            active = Token(Token.ACTIVE, line_no, m.start(2), m.group(2))
            text = Token(Token.TEXT, line_no, m.start(3), m.group(3))
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            newline = Token(Token.NEWLINE, line_no, m.start(5))
            return [level4, active, text] + metadata + [newline]
        else:
            raise LexerException("Bad subproject entry.", line_no)

    def _tokenize_line_task(self, line_no, line):
        m = Token.TASK_RE.match(line)
        if m:
            task = Token(Token.TASK, line_no, m.start(1), m.group(1))
            complete = Token(Token.COMPLETE, line_no, m.start(2), m.group(2))
            text = Token(Token.TEXT, line_no, m.start(3), m.group(3))
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            newline = Token(Token.NEWLINE, line_no, m.start(5))
            return [task, complete, text] + metadata + [newline]
        else:
            raise LexerException("Bad task entry.", line_no)

    def _tokenize_line_subtask(self, line_no, line):
        m = Token.SUBTASK_RE.match(line)
        if m:
            subtask = Token(Token.SUBTASK, line_no, m.start(1), m.group(1))
            complete = Token(Token.COMPLETE, line_no, m.start(2), m.group(2))
            text = Token(Token.TEXT, line_no, m.start(3), m.group(3))
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            newline = Token(Token.NEWLINE, line_no, m.start(5))
            return [subtask, complete, text] + metadata + [newline]
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

    def _tokenize_metadata(self, line, col, lexeme):
        if lexeme:
            return [Token(Token.METADATA, line, col, lexeme)]
        else:
            return []


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
        if self.tokens:
            if minimum_length > len(self.tokens):
                last_token = self.tokens[-1]
                fmt = "Unexpected end of token stream. Last token in: {}"
                msg = fmt.format(last_token)
                raise ParserException(msg, last_token)
        else:
            msg = "Unexpected end of token stream. No tokens left."
            raise ParserException(msg)

    def _Start(self):
        """
        _Start : _GoalsEntry _RoutineEntry
        """
        goals = self._GoalsEntry()
        routine = self._RoutineEntry()
        return Todo(goals, routine)

    def _GoalsEntry(self):
        """
        _GoalsEntry : LEVEL1 GOALS NEWLINE _Goals
        """
        self._check_token_stream_length(3)

        level1_token = self.tokens.popleft()
        if not level1_token.is_of_type(Token.LEVEL1):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL1_LEXEME, level1_token.lexeme)
            raise ParserException(msg, level1_token)

        goals_token = self.tokens.popleft()
        if not goals_token.is_of_type(Token.GOALS):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.GOALS_LEXEME, goals_token.lexeme)
            raise ParserException(msg, goals_token)

        newline_token = self.tokens.popleft()
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
        goal = self._Goal()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL2):
            goals = self._Goals()
            return [goal] + goals
        else:
            return [goal]

    def _Goal(self):
        """
        _Goal : LEVEL2 ACTIVE TEXT NEWLINE _Projects
        """
        self._check_token_stream_length(4)

        level2_token = self.tokens.popleft()
        if not level2_token.is_of_type(Token.LEVEL2):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL2_LEXEME, level2_token.lexeme)
            raise ParserException(msg, level2_token)

        active_token = self.tokens.popleft()
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected goal active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        active = active_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected goal name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        newline_token = self.tokens.popleft()
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        projects = self._Projects()
        return Goal(name, active, projects)

    def _Projects(self):
        """
        _Projects : _Project
                  | _Project Projects
        """
        project = self._Project()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL3):
            projects = self._Projects()
            return [project] + projects
        else:
            return [project]

    def _Project(self):
        """
        _Project : LEVEL3 ACTIVE TEXT NEWLINE
                 | LEVEL3 ACTIVE TEXT NEWLINE _ProjectChildren
                 | LEVEL3 ACTIVE TEXT METADATA NEWLINE
                 | LEVEL3 ACTIVE TEXT METADATA NEWLINE _ProjectChildren
        """
        self._check_token_stream_length(4)

        level3_token = self.tokens.popleft()
        if not level3_token.is_of_type(Token.LEVEL3):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL3_LEXEME, level3_token.lexeme)
            raise ParserException(msg, level3_token)

        active_token = self.tokens.popleft()
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected project active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        active = active_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected project name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self.tokens.popleft()
            if not newline_token.is_of_type(Token.NEWLINE):
                fmt = "Expected newline, found '{}'."
                msg = fmt.format(Token.newline_token.lexeme)
                raise ParserException(msg, newline_token)
        else:
            fmt = "Expected metadata or newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        subprojects = tasks = []
        children = self.tokens and (self.tokens[0].is_of_type(Token.LEVEL4) or
                                    self.tokens[0].is_of_type(Token.TASK))
        if children:
            subprojects, tasks = self._ProjectChildren()

        return Project(name, active, subprojects, tasks, metadata)

    def _ProjectChildren(self):
        """
        _ProjectChildren : _SubProjects
                         | _Tasks
        """
        subprojects = tasks = []
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
        subproject = self._Subproject()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL4):
            subprojects = self._Subprojects()
            return [subproject] + subprojects
        else:
            return [subproject]

    def _Subproject(self):
        """
        _Subproject : LEVEL4 ACTIVE TEXT NEWLINE
                    | LEVEL4 ACTIVE TEXT NEWLINE _Tasks
                    | LEVEL4 ACTIVE TEXT METADATA NEWLINE
                    | LEVEL4 ACTIVE TEXT METADATA NEWLINE _Tasks
        """
        self._check_token_stream_length(4)

        level4_token = self.tokens.popleft()
        if not level4_token.is_of_type(Token.LEVEL4):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL4_LEXEME, level4_token.lexeme)
            raise ParserException(msg, level4_token)

        active_token = self.tokens.popleft()
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected subproject active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        active = active_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected subproject name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self.tokens.popleft()
            if not newline_token.is_of_type(Token.NEWLINE):
                fmt = "Expected newline, found '{}'."
                msg = fmt.format(Token.newline_token.lexeme)
                raise ParserException(msg, newline_token)
        else:
            fmt = "Expected metadata or newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        tasks = []
        if self.tokens and self.tokens[0].is_of_type(Token.TASK):
            tasks = self._Tasks()

        return Subproject(name, active, tasks, metadata)

    def _Tasks(self):
        """
        _Tasks : _Task
               | _Tasks
        """
        task = self._Task()
        if self.tokens and self.tokens[0].is_of_type(Token.TASK):
            tasks = self._Tasks()
            return [task] + tasks
        else:
            return [task]

    def _Task(self):
        """
        _Task : TASK COMPLETE TEXT NEWLINE
              | TASK COMPLETE TEXT NEWLINE _Subtasks
              | TASK COMPLETE TEXT METADATA NEWLINE
              | TASK COMPLETE TEXT METADATA NEWLINE _Subtasks
        """
        self._check_token_stream_length(4)

        task_token = self.tokens.popleft()
        if not task_token.is_of_type(Token.TASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.TASK_LEXEME, task_token.lexeme)
            raise ParserException(msg, task_token)

        complete_token = self.tokens.popleft()
        if not complete_token.is_of_type(Token.COMPLETE):
            fmt = "Expected task completion indicator, found '{}'."
            msg = fmt.format(complete_token.lexeme)
            raise ParserException(msg, complete_token)
        complete = complete_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected task name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self.tokens.popleft()
            if not newline_token.is_of_type(Token.NEWLINE):
                fmt = "Expected newline, found '{}'."
                msg = fmt.format(Token.newline_token.lexeme)
                raise ParserException(msg, newline_token)
        else:
            fmt = "Expected metadata or newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        subtasks = []
        if self.tokens and self.tokens[0].is_of_type(Token.SUBTASK):
            subtasks = self._Subtasks()

        return Task(name, complete, subtasks, metadata)

    def _Subtasks(self):
        """
        _Subtasks : _Subtask
                  | _Subtasks
        """
        subtask = self._Subtask()
        if self.tokens and self.tokens[0].is_of_type(Token.SUBTASK):
            subtasks = self._Subtasks()
            return [subtask] + subtasks
        else:
            return [subtask]

    def _Subtask(self):
        """
        _Subtask : SUBTASK COMPLETE TEXT NEWLINE
                 | SUBTASK COMPLETE TEXT METADATA NEWLINE
        """
        self._check_token_stream_length(4)

        subtask_token = self.tokens.popleft()
        if not subtask_token.is_of_type(Token.SUBTASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.SUBTASK_LEXEME, subtask_token.lexeme)
            raise ParserException(msg, subtask_token)

        complete_token = self.tokens.popleft()
        if not complete_token.is_of_type(Token.COMPLETE):
            fmt = "Expected subtask completion indicator, found '{}'."
            msg = fmt.format(complete_token.lexeme)
            raise ParserException(msg, complete_token)
        complete = complete_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected subtask name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self.tokens.popleft()
            if not newline_token.is_of_type(Token.NEWLINE):
                fmt = "Expected newline, found '{}'."
                msg = fmt.format(Token.newline_token.lexeme)
                raise ParserException(msg, newline_token)
        else:
            fmt = "Expected metadata or newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        return Subtask(name, complete, metadata)

    def _RoutineEntry(self):
        """
        _RoutineEntry : LEVEL1 ROUTINE NEWLINE _Week
        """
        self._check_token_stream_length(3)

        level1_token = self.tokens.popleft()
        if not level1_token.is_of_type(Token.LEVEL1):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL1_LEXEME, level1_token.lexeme)
            raise ParserException(msg, level1_token)

        routine_token = self.tokens.popleft()
        if not routine_token.is_of_type(Token.ROUTINE):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.ROUTINE_LEXEME, routine_token.lexeme)
            raise ParserException(msg, routine_token)

        newline_token = self.tokens.popleft()
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        days = self._Week()
        return Routine(days)

    def _Week(self):
        """
        _Week : _Day _Day _Day _Day _Day _Day _Day
        """
        day1 = self._Day()
        day2 = self._Day()
        day3 = self._Day()
        day4 = self._Day()
        day5 = self._Day()
        day6 = self._Day()
        day7 = self._Day()

        return [day1, day2, day3, day4, day5, day6, day7]

    def _Day(self):
        """
        _Day : LEVEL2 TEXT NEWLINE _Slots
        """
        self._check_token_stream_length(3)

        level2_token = self.tokens.popleft()
        if not level2_token.is_of_type(Token.LEVEL2):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL2_LEXEME, level2_token.lexeme)
            raise ParserException(msg, level2_token)

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected day name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        name = text_token.value

        newline_token = self.tokens.popleft()
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        slots = self._Slots()

        return Day(name, slots)

    def _Slots(self):
        """
        _Slots : _Slot
               | _Slot _Slots
        """
        slot = self._Slot()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL3):
            slots = self._Slots()
            return [slot] + slots
        else:
            return [slot]

    def _Slot(self):
        """
        _Slot : LEVEL3 TIME POMODOROS TEXT NEWLINE
        """
        self._check_token_stream_length(4)

        level3_token = self.tokens.popleft()
        if not level3_token.is_of_type(Token.LEVEL3):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL2_LEXEME, level3_token.lexeme)
            raise ParserException(msg, level3_token)

        time_token = self.tokens.popleft()
        if not time_token.is_of_type(Token.TIME):
            fmt = "Expected task name, found '{}'."
            msg = fmt.format(time_token.lexeme)
            raise ParserException(msg, time_token)
        time = time_token.value

        pomodoros_token = self.tokens.popleft()
        if not pomodoros_token.is_of_type(Token.POMODOROS):
            fmt = "Expected integer, found '{}'."
            msg = fmt.format(pomodoros_token.lexeme)
            raise ParserException(msg, pomodoros_token)
        pomodoros = pomodoros_token.value

        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected todo item name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        todo = text_token.value

        newline_token = self.tokens.popleft()
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)

        return Slot(time, pomodoros, todo)


class Schedule:

    def __init__(self, todo):
        def roundrobin(*iterables):
            pending = len(iterables)
            nexts = cycle(iter(it).__next__ for it in iterables)
            while pending:
                try:
                    for next in nexts:
                        yield next()
                except StopIteration:
                    pending -= 1
                    nexts = cycle(islice(nexts, pending))

        def build_candidate_list(tasklist, category):
            l = [tasklist[key] for key in tasklist if category in key]
            return [x for x in roundrobin(*l)]

        # shift routine to start on today
        todo.routine.shift()
        self.schedule = copy.copy(todo.routine.days)
        active_goals = todo.goals.get_active()

        for day in self.schedule:
            for slot in day.slots:
                # get a task/subtask for this slot and assign it
                print("XXX", slot)

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        return ""


def main():
    sys.setrecursionlimit(2000)

    filename = sys.argv[1]
    try:
        lexer = OrgLexer(filename)
        tokens = lexer.tokenize()

        parser = OrgParser(tokens)
        todo = parser.parse()

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
