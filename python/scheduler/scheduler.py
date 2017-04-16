#!/usr/bin/env python3

# TODO / GOALS / PROJECTS / SUBPROJECTS / TASKS / SUBTASKS
# ROUTINE / DAY / SLOT / SCHEDULE
#
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
#
# GRAMMAR:
#   _Start           : _LogsEntry _GoalsEntry _RoutineEntry
#   _LogsEntry       : LEVEL1 LOGS NEWLINE _Logs
#   _Logs            : _Log
#                    | _Log Logs
#   _Log             : TEXT NEWLINE
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
import itertools


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
        asp = [sp.get_incomplete() for sp in self.subprojects if sp.active]
        it = [t.get_incomplete() for t in self.tasks if not t.complete]
        return Project(self.name, self.active, asp, it, self.metadata)


class Subproject:
    def __init__(self, name, active, tasks=[], metadata={}):
        self.name = name
        self.active = active
        self.tasks = tasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        active = "-"
        if self.active:
            active = "+"
        self_str = "**** {} {}\n".format(active, self.name)
        if self.tasks:
            tasks_str = "\n".join([str(t) for t in self.tasks])
            return self_str + tasks_str
        else:
            return self_str

    def get_incomplete(self):
        it = [t.get_incomplete() for t in self.tasks if (not t.complete)]
        return Subproject(self.name, self.active, it, self.metadata)


class Task:
    def __init__(self, name, complete, subtasks=[], metadata={}):
        self.name = name
        self.complete = complete
        self.subtasks = subtasks
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        complete = " "
        if self.complete:
            complete = "X"
        self_str = "- [{}] {}".format(complete, self.name)
        if self.subtasks:
            subtasks_str = "\n" + "\n".join([str(st) for st in self.subtasks])
            return self_str + subtasks_str
        else:
            return self_str

    def get_incomplete(self):
        ist = [st for st in self.subtasks if (not st.complete)]
        return Task(self.name, self.complete, ist, self.metadata)


class Subtask:
    def __init__(self, name, complete, metadata={}):
        self.name = name
        self.complete = complete
        self.metadata = metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        complete = " "
        if self.complete:
            complete = "X"
        return "  - [{}] {}".format(complete, self.name)


class Routine:
    def __init__(self, days):

        self.days = []
        delta = datetime.timedelta(minutes=30)
        for routine_day in days:
            schedule_day = Day(routine_day.name, [])
            for slot in routine_day.slots:
                h = slot.hour
                m = slot.minute
                today = datetime.datetime.today()
                base_time = today.replace(hour=h, minute=m,
                                          second=0, microsecond=0)
                for i in range(slot.pomodoros):
                    slot_time = base_time + delta * i
                    start_time = slot_time.strftime("%H:%M")
                    schedule_slot = Slot(slot_time.hour, slot_time.minute, 1,
                                         slot.item_name, start_time)
                    schedule_day.slots.append(schedule_slot)
            self.days.append(schedule_day)

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def shift(self):
        shifts = datetime.datetime.isoweekday(datetime.datetime.today()) - 1
        for i in range(shifts):
            self.days = self.days[1:] + self.days[:1]


class Day:
    def __init__(self, name="", slots=[]):
        self.name = name
        self.slots = slots

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)


class Slot:
    def __init__(self, hour, minute, pomodoros, item_name, start_time=None):
        self.hour = hour
        self.minute = minute
        self.pomodoros = pomodoros
        self.item_name = item_name
        self.start_time = start_time
        self.item = None

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
    LOGS = "LOGS"
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
        LOGS,
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
    LOGS_LEXEME = "LOGS"
    GOALS_LEXEME = "GOALS"
    ROUTINE_LEXEME = "ROUTINE"
    TASK_LEXEME = "- "
    SUBTASK_LEXEME = "  - "

    LOGS_RE = re.compile("(\*) (LOGS)(\n)")
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
                    hour = int(hm[0])
                    minute = int(hm[1])
                    self.value = (hour, minute)
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
        if line_rest.startswith(Token.LOGS_LEXEME):
            m = Token.LOGS_RE.match(line)
            if m:
                level1 = Token(Token.LEVEL1, line_no, m.start(1), m.group(1))
                logs = Token(Token.LOGS, line_no, m.start(2), m.group(2))
                newline = Token(Token.NEWLINE, line_no)
                return [level1, logs, newline]
            else:
                raise LexerException("Bad top-level logs entry.", line_no)
        elif line_rest.startswith(Token.GOALS_LEXEME):
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
        m = Token.PROJECT_RE.match(line)
        n = Token.SLOT_RE.match(line)
        if m:  # Project entry
            level3 = Token(Token.LEVEL3, line_no, m.start(1), m.group(1))
            active = Token(Token.ACTIVE, line_no, m.start(2), m.group(2))
            text = Token(Token.TEXT, line_no, m.start(3), m.group(3))
            metadata = self._tokenize_metadata(line_no, m.start(4), m.group(4))
            newline = Token(Token.NEWLINE, line_no, m.start(5))
            return [level3, active, text] + metadata + [newline]
        elif n:  # Routine slot entry
            level3 = Token(Token.LEVEL3, line_no, n.start(1), n.group(1))
            time = Token(Token.TIME, line_no, n.start(2), n.group(2))
            pomodoros = Token(Token.POMODOROS, line_no, n.start(3), n.group(3))
            todo = Token(Token.TEXT, line_no, n.start(4), n.group(4))
            newline = Token(Token.NEWLINE, line_no, n.start(5))
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

    def _tokenize_line_text(self, line_no, line):
        text = Token(Token.TEXT, line_no, 0, line[:-1])
        newline = Token(Token.NEWLINE, line_no, len(line) - 1)
        return [text, newline]

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
            # fmt = "Bad entry: {}:{}"
            # raise LexerException(fmt.format(line_no, line), line_no)
            return self._tokenize_line_text(line_no, line)

    def _tokenize_metadata(self, line, col, lexeme):
        if lexeme:
            return [Token(Token.METADATA, line, col, lexeme)]
        else:
            return []

    def _tokenize_duration(self, lexeme):
        return None


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

    def _get_logs_token(self):
        logs_token = self.tokens.popleft()
        if not logs_token.is_of_type(Token.LOGS):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LOGS_LEXEME, logs_token.lexeme)
            raise ParserException(msg, logs_token)
        return logs_token

    def _get_goals_token(self):
        goals_token = self.tokens.popleft()
        if not goals_token.is_of_type(Token.GOALS):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.GOALS_LEXEME, goals_token.lexeme)
            raise ParserException(msg, goals_token)
        return goals_token

    def _get_routine_token(self):
        routine_token = self.tokens.popleft()
        if not routine_token.is_of_type(Token.ROUTINE):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.ROUTINE_LEXEME, routine_token.lexeme)
            raise ParserException(msg, routine_token)
        return routine_token

    def _get_level1_token(self):
        level1_token = self.tokens.popleft()
        if not level1_token.is_of_type(Token.LEVEL1):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL1_LEXEME, level1_token.lexeme)
            raise ParserException(msg, level1_token)
        return level1_token

    def _get_level2_token(self):
        level2_token = self.tokens.popleft()
        if not level2_token.is_of_type(Token.LEVEL2):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL2_LEXEME, level2_token.lexeme)
            raise ParserException(msg, level2_token)
        return level2_token

    def _get_level3_token(self):
        level3_token = self.tokens.popleft()
        if not level3_token.is_of_type(Token.LEVEL3):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL3_LEXEME, level3_token.lexeme)
            raise ParserException(msg, level3_token)
        return level3_token

    def _get_level4_token(self):
        level4_token = self.tokens.popleft()
        if not level4_token.is_of_type(Token.LEVEL4):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.LEVEL4_LEXEME, level4_token.lexeme)
            raise ParserException(msg, level4_token)
        return level4_token

    def _get_task_token(self):
        task_token = self.tokens.popleft()
        if not task_token.is_of_type(Token.TASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.TASK_LEXEME, task_token.lexeme)
            raise ParserException(msg, task_token)
        return task_token

    def _get_subtask_token(self):
        subtask_token = self.tokens.popleft()
        if not subtask_token.is_of_type(Token.SUBTASK):
            fmt = "Expected '{}', found '{}'."
            msg = fmt.format(Token.SUBTASK_LEXEME, subtask_token.lexeme)
            raise ParserException(msg, subtask_token)
        return subtask_token

    def _get_active_token(self):
        active_token = self.tokens.popleft()
        if not active_token.is_of_type(Token.ACTIVE):
            fmt = "Expected project active indicator, found '{}'."
            msg = fmt.format(active_token.lexeme)
            raise ParserException(msg, active_token)
        return active_token

    def _get_complete_token(self):
        complete_token = self.tokens.popleft()
        if not complete_token.is_of_type(Token.COMPLETE):
            fmt = "Expected task completion indicator, found '{}'."
            msg = fmt.format(complete_token.lexeme)
            raise ParserException(msg, complete_token)
        return complete_token

    def _get_text_token(self):
        text_token = self.tokens.popleft()
        if not text_token.is_of_type(Token.TEXT):
            fmt = "Expected subproject name, found '{}'."
            msg = fmt.format(text_token.lexeme)
            raise ParserException(msg, text_token)
        return text_token

    def _get_time_token(self):
        time_token = self.tokens.popleft()
        if not time_token.is_of_type(Token.TIME):
            fmt = "Expected task name, found '{}'."
            msg = fmt.format(time_token.lexeme)
            raise ParserException(msg, time_token)
        return time_token

    def _get_pomodoros_token(self):
        pomodoros_token = self.tokens.popleft()
        if not pomodoros_token.is_of_type(Token.POMODOROS):
            fmt = "Expected integer, found '{}'."
            msg = fmt.format(pomodoros_token.lexeme)
            raise ParserException(msg, pomodoros_token)
        return pomodoros_token

    def _get_newline_token(self):
        newline_token = self.tokens.popleft()
        if not newline_token.is_of_type(Token.NEWLINE):
            fmt = "Expected newline, found '{}'."
            msg = fmt.format(Token.newline_token.lexeme)
            raise ParserException(msg, newline_token)
        return newline_token

    def _Start(self):
        """
        _Start : _Logs _GoalsEntry _RoutineEntry
        """
        logs = self._LogsEntry()
        goals = self._GoalsEntry()
        routine = self._RoutineEntry()
        return Todo(goals, routine)

    def _LogsEntry(self):
        """
        _LogsEntry : LEVEL1 LOGS NEWLINE _Logs
        """
        self._check_token_stream_length(3)

        level1_token = self._get_level1_token()
        logs_token = self._get_logs_token()
        newline_token = self._get_newline_token()

        return self._Logs()

    def _Logs(self):
        """
        _Logs : _Log
              | _Log Logs
        """
        log = self._Log()
        if self.tokens and self.tokens[0].is_of_type(Token.LEVEL1):
            return None
        else:
            return self._Logs()

    def _Log(self):
        """
        _Log : TEXT NEWLINE
        """
        self._check_token_stream_length(2)

        text_token = self._get_text_token()
        newline_token = self._get_newline_token()
        return None

    def _GoalsEntry(self):
        """
        _GoalsEntry : LEVEL1 GOALS NEWLINE _Goals
        """
        self._check_token_stream_length(3)

        level1_token = self._get_level1_token()
        goals_token = self._get_goals_token()
        newline_token = self._get_newline_token()

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

        level2_token = self._get_level2_token()
        active_token = self._get_active_token()
        text_token = self._get_text_token()
        newline_token = self._get_newline_token()

        projects = self._Projects()

        active = active_token.value
        name = text_token.value

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

        level3_token = self._get_level3_token()
        active_token = self._get_active_token()
        text_token = self._get_text_token()

        active = active_token.value
        name = text_token.value
        metadata = {}

        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self._get_newline_token()
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

        level4_token = self._get_level4_token()
        active_token = self._get_active_token()
        text_token = self._get_text_token()

        active = active_token.value
        name = text_token.value

        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self._get_newline_token()
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

        task_token = self._get_task_token()
        complete_token = self._get_complete_token()
        text_token = self._get_text_token()

        complete = complete_token.value
        name = text_token.value
        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self._get_newline_token()
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

        subtask_token = self._get_subtask_token()
        complete_token = self._get_complete_token()
        text_token = self._get_text_token()

        complete = complete_token.value
        name = text_token.value
        metadata = {}
        token = self.tokens.popleft()
        if token.is_of_type(Token.NEWLINE):
            pass
        elif token.is_of_type(Token.METADATA):
            metadata = token.value
            newline_token = self._get_newline_token()
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

        level1_token = self._get_level1_token()
        routine_token = self._get_routine_token()
        newline_token = self._get_newline_token()

        days = self._Week()
        return Routine(days)

    def _Week(self):
        """
        _Week : _Day _Day _Day _Day _Day _Day _Day
        """
        return [self._Day() for _ in range(7)]

    def _Day(self):
        """
        _Day : LEVEL2 TEXT NEWLINE _Slots
        """
        self._check_token_stream_length(3)

        level2_token = self._get_level2_token()
        text_token = self._get_text_token()
        newline_token = self._get_newline_token()

        slots = self._Slots()

        name = text_token.value
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

        level3_token = self._get_level3_token()
        time_token = self._get_time_token()
        pomodoros_token = self._get_pomodoros_token()
        text_token =  self._get_text_token()
        newline_token = self._get_newline_token()

        hour, minute = time_token.value
        pomodoros = pomodoros_token.value
        item_name = text_token.value

        return Slot(hour, minute, pomodoros, item_name)


class Schedule:
    def __init__(self, todo):

        def roundrobin(*iterables):
            pending = len(iterables)
            nexts = itertools.cycle(iter(it).__next__ for it in iterables)
            while pending:
                try:
                    for next in nexts:
                        yield next()
                except StopIteration:
                    pending -= 1
                    nexts = itertools.cycle(itertools.islice(nexts, pending))

        def get_todo_items(todo):
            # get the set of todo items in the routine, which may be goal,
            # project or subproject names
            todo_items = set()
            for day in todo.routine.days:
                for slot in day.slots:
                    todo_items.add(slot.item_name)
            return todo_items

        def todo_to_list(todo):

            def add_item(todo_list, goal, project, subproject, task, item):
                item.metadata["goal"] = goal
                item.metadata["project"] = project
                item.metadata["subproject"] = subproject
                if type(item) is Subtask:
                    item.metadata["task"] = task
                key = (goal, project, subproject)
                if key not in todo_list:
                    todo_list[key] = []
                todo_list[key].append(item)

            def add_items(todo_list, goal, project, subproject, task, items):
                for item in items:
                    add_item(todo_list, goal, project, subproject, task, item)

            def add_task(todo_list, goal, project, subproject, task):
                if task.subtasks:
                    # task has subtasks, add them
                    add_items(todo_list, goal, project, subproject,
                              task.name, task.subtasks)
                else:
                    # task has no subtasks, add it
                    add_items(todo_list, goal, project, subproject,
                              '', [task])

            def add_project(todo_list, goal, project):
                if project.subprojects:
                    for subproject in project.subprojects:
                        for task in subproject.tasks:
                            g = goal.name
                            p = project.name
                            sp = subproject.name
                            add_task(todo_list, g, p, sp, task)
                if project.tasks:
                    for task in project.tasks:
                        add_task(todo_list, goal.name, project.name, '', task)

            todo_active = todo.get_active()
            todo_list = collections.OrderedDict()
            for goal in todo_active.goals:
                for project in goal.projects:
                    add_project(todo_list, goal, project)
            return todo_list

        def candidate_todo_items(todo_list, todo_items):
            candidate_list = {}
            for todo_item in todo_items:
                lst = [todo_list[key] for key in todo_list if todo_item in key]
                candidate_list[todo_item] = [x for x in roundrobin(*lst)]
            return candidate_list

        # shift routine to start on today
        todo.routine.shift()
        self.schedule = copy.copy(todo.routine)

        todo_items = get_todo_items(todo)
        todo_list = todo_to_list(todo)
        candidates = candidate_todo_items(todo_list, todo_items)

        for day in self.schedule.days:
            for slot in day.slots:
                # get a task/subtask for this slot and assign it
                category = slot.item_name
                category_items = candidates[category]
                if category_items != []:
                    item = category_items[0]
                    slot.item = item
                    if "pomodoros" in item.metadata:
                        if item.metadata["pomodoros"] == 1:
                            category_items.remove(item)
                        else:
                            item.metadata["pomodoros"] -= 1
                    # By commenting this out, tasks will continue to be
                    # scheduled until they are complete. Otherwise they are
                    # scheduler for a single pomodoro.
                    # else:
                    #     category_items.remove(item)

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def __str__(self):
        s = ""
        goals = set(["goal"])
        projects = set(["project"])
        subprojects = set(["subproject"])

        days = self.schedule.days
        for day in days:
            for slot in day.slots:
                if slot.item:
                    if "goal" in slot.item.metadata:
                        goals.add(slot.item.metadata["goal"])
                    if "project" in slot.item.metadata:
                        projects.add(slot.item.metadata["project"])
                    if "subproject" in slot.item.metadata:
                        subprojects.add(slot.item.metadata["subproject"])

        maxd = str(max([len("day")] + [len(d.name) for d in days]))
        maxg = str(max([len(g) for g in goals]))
        maxp = str(max([len(p) for p in projects]))
        maxsp = str(max([len(sp) for sp in subprojects]))

        dfmt = "{: >" + maxd + "} {} | "
        gfmt = "{: >" + maxg + "} | "
        pfmt = "{: >" + maxp + "} | "
        spfmt = "{: >" + maxsp + "} | "
        fmt = dfmt + gfmt + pfmt + spfmt + "{}\n"
        s += fmt.format("day", "time ", "goal", "project", "subproject",
                        "task")

        for day in days:
            d = day.name
            for slot in day.slots:
                if slot.item:
                    g = slot.item.metadata["goal"]
                    p = slot.item.metadata["project"]
                    sp = slot.item.metadata["subproject"]
                    fmt = dfmt + gfmt + pfmt + spfmt
                    if type(slot.item) is Task:
                        fmt += "{}\n"
                        t = slot.item.name
                        s += fmt.format(d, slot.start_time, g, p, sp, t)
                    elif type(slot.item) is Subtask:
                        fmt += "{}:{}\n"
                        t = slot.item.metadata["task"]
                        st = slot.item.name
                        s += fmt.format(d, slot.start_time, g, p, sp, t, st)
                    else:
                        print("ERROR: ", slot.item)
                        sys.exit(1)
        return s


def usage():
    USAGE = """
    scheduler.py COMMAND FILE
    COMMAND = active | schedule
    """
    print(USAGE)


def main(command, filename):
    sys.setrecursionlimit(2000)
    try:
        lexer = OrgLexer(filename)
        tokens = lexer.tokenize()
        parser = OrgParser(tokens)
        todo = parser.parse()
    except LexerException as e:
        print(e.message)
        exit(1)
    except ParserException as e:
        print(e.message)
        exit(1)

    if "week" == command:
        schedule = Schedule(todo)
        print(schedule)
    elif 'today' == command:
        schedule = Schedule(todo)
        schedule.schedule.days = schedule.schedule.days[0:1]
        print(schedule)
    elif "active" == command:
        active_todo = todo.get_active()
        print(active_todo)
    else:
        usage()
        exit(1)


if __name__ == "__main__":
    if 3 == len(sys.argv):
        main(sys.argv[1], sys.argv[2])
    else:
        usage()
        exit(1)
