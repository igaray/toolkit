#!/usr/bin/python3
"""
todo
  - cambiar parseo para soportar orgmode
  - hacer descomposicion de tareas
  - asignar duraciones realistas
  - hacer lugar en la rutina para compras, mandados, cocinar, limpiar y lavar, ocio, etc
  - poner rutina diaria en el calendario
  - agregar charlas y videos
  - agregar libros de casa que quiero leer
  - agregar libros digitales que quiero leer
  - agregar series y peliculas
  - agregar cursos de coursera y edx
  - ver como representar dependencias y tenerlas en cuenta en el algoritmo
  - ver como representar due dates y tenerlas en cuenta en el algoritmo
"""
import collections
import copy
import datetime
import json
import os
import pprint
import string
import sys
from itertools import cycle, islice

def jdefault(object):
    return object.__dict__

class TableParser():
    def __init__(self):
        pass

class OrgModeParser():
    def __init__(self):
        pass

#-------------------------------------------------------------------------------
class TableParser(Parser):

    def __init__(self):
        pass

    def parse(filename):
        data = [[], [], []]

        with open(self.filename, "r") as todofile:
            i = 0
            for line in todofile:
                if line == "%%\n":
                    i += 1
                else:
                    data[i].append(line)

        todo_data = data[0]
        routine_data = data[1]

        return (todo_data, routine_data)

#-------------------------------------------------------------------------------
class OrgParser:

    def __init__(self):
        pass

    def parse(filename):
        with open(self.filename, "r") as todofile:
            lines = todofile.readlines()
            self._Start(lines)

#-------------------------------------------------------------------------------
class CSVParser:

    def __init__(self):
        pass

    def parse(filename):
        with open(self.filename, "r") as todofile:
            pass

#-------------------------------------------------------------------------------
class RoutineEntry:
    # PARSE
    def __init__(self, line):
        self.fields = [field.strip(" <>") for field in line[:-1].split("|")]
        self.day = ''
        self.start = ''
        self.duration = 0
        self.category = ''
        if len(self.fields) == 4 and not self.is_header():
            self.day = self.fields[0]
            self.start = [int(x) for x in self.fields[1].split(':')]
            self.duration = int(self.fields[2])
            self.category = self.fields[3]

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_empty(self):
        return (self.fields == []) or (self.fields == ['']) or (self.fields == ['', '', '', ''])

    def is_header(self):
        return self.fields[0] == 'routine'

    def is_day(self):
        return self.fields[0] in ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday']

#-------------------------------------------------------------------------------
class TodoEntry:
    # PARSE
    def __init__(self, line):
        self.fields = [field.strip(" <>") for field in line[:-1].split("|")]
        self.goal = ''
        self.project = ''
        self.subproject = ''
        self.task = ''
        self.subtask = ''
        self.metadata_raw = ''
        self.metadata = {}

        if len(self.fields) == 6:
            for field in self.fields:
                if len(field) > 2 and (field[:2] in ['+ ', '- ']):
                    if field[0] == '+':
                        self.metadata["active"] = True
                    elif field[0] == '-':
                        self.metadata["active"] = False
                    else:
                        print("ERROR: ", self)
                        sys.exit(1)
                    # field = field[2:]
                    break
                if len(field) > 4 and (field[:4] in['[ ] ', '[x] ']):
                    if field[:3] == '[x]':
                        self.metadata["done"] = True
                    elif field[:3] == '[ ]':
                        self.metadata["done"] = False
                    else:
                        print("ERROR: ", self)
                        sys.exit(1)
                    break
                    # field = field[4:]

            self.goal = self.fields[0][2:]
            self.project = self.fields[1][2:]
            self.subproject = self.fields[2][2:]
            self.task = self.fields[3][4:]
            self.subtask = self.fields[4][4:]

            self.metadata_raw = self.fields[5]
            if self.metadata_raw != '':
                metadata_fields = [field.split(':') for field in self.metadata_raw.split(',')]
                for mdfield in metadata_fields:
                    if len(mdfield)  == 2:
                        self.metadata[mdfield[0]] = mdfield[1]

            if "duration" in self.metadata:
                self.metadata["duration"] = int(self.metadata["duration"])
            else:
                self.metadata["duration"] = 1

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_empty(self):
        return (self.fields == []) or (self.fields == ['']) or (self.fields == ['', '', '', '', '', ''])

    def is_header(self):
        return self.fields[0] == 'goals'

    def is_goal(self):
        return self.goal != ''

    def is_project(self):
        return self.project != ''

    def is_subproject(self):
        return self.subproject != ''

    def is_task(self):
        return self.task != ''

    def is_subtask(self):
        return self.subtask != ''

#-------------------------------------------------------------------------------
class Goal:
    def __init__(self, entry):
        self.name = entry.goal
        self.metadata = entry.metadata
        self.projects = []

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_active(self):
        return self.metadata["active"]

    def active(self):
        c = copy.copy(self)
        c.projects = [project.active() for project in self.projects if project.is_active()]
        return c

    def add_project(self, project):
        self.projects.append(project)

#-------------------------------------------------------------------------------
class Project:
    def __init__(self, entry):
        self.name = entry.project
        self.metadata = entry.metadata
        self.subitems = []

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_active(self):
        return self.metadata["active"]

    def active(self):
        c = copy.copy(self)
        c.subitems = [item.active() for item in self.subitems if item.is_active()]
        return c

    def has_subitems(self):
        return self.subitems != []

    def add_item(self, item):
        self.subitems.append(item)

#-------------------------------------------------------------------------------
class Subproject:
    def __init__(self, entry):
        self.name = entry.subproject
        self.metadata = entry.metadata
        self.subitems = []

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_active(self):
        return self.metadata["active"]

    def active(self):
        c = copy.copy(self)
        c.subitems = [item.active() for item in self.subitems if item.is_active()]
        return c

    def has_subitems(self):
        return self.subitems != []

    def add_item(self, item):
        self.subitems.append(item)

#-------------------------------------------------------------------------------
class Task:
    def __init__(self, entry, goal, project, subproject):
        self.name = entry.task
        self.goal = ""
        if goal:
            self.goal = goal.name
        self.project = ""
        if project:
            self.project = project.name
        self.subproject = ""
        if subproject:
            self.subproject = subproject.name
        self.subitems = []
        self.metadata = entry.metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_active(self):
        return not self.metadata["done"]

    def active(self):
        c = copy.copy(self)
        c.subitems = [item for item in self.subitems if item.is_active()]
        return c

    def has_subitems(self):
        return self.subitems != []

    def add_item(self, item):
        self.subitems.append(item)

#-------------------------------------------------------------------------------
class Subtask:
    def __init__(self, entry, goal, project, subproject, task):
        self.name = entry.subtask
        self.goal = ""
        if goal:
            self.goal = goal.name
        self.project = ""
        if project:
            self.project = project.name
        self.subproject = ""
        if subproject:
            self.subproject = subproject.name
        self.task = ""
        if task:
            self.task = task.name
        self.metadata = entry.metadata

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def is_active(self):
        return not self.metadata["done"]

#-------------------------------------------------------------------------------
class Todo:
    def __init__(self, raw_data):
        data = []
        for line in raw_data:
            entry = TodoEntry(line)
            if not entry.is_empty():
                data.append(entry)

        self.goals = []

        self.current_goal = None
        self.current_project = None
        self.current_subproject = None
        self.current_task = None

        for entry in data:
            if entry.is_header():
                pass
            elif entry.is_goal():
                self.add_goal(entry)
            elif entry.is_project():
                self.add_project(entry)
            elif entry.is_subproject():
                self.add_subproject(entry)
            elif entry.is_task():
                self.add_task(entry)
            elif entry.is_subtask():
                self.add_subtask(entry)
            else:
                print("ERROR entry: {}".format(entry))
                sys.exit(1)

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def output(self):
        print("TODO:")
        print(self)

    def add_goal(self, entry):
        goal = Goal(entry)
        self.goals.append(goal)
        self.current_goal = goal
        self.current_project = None
        self.current_subproject = None
        self.current_task = None

    def add_project(self, entry):
        project = Project(entry)
        self.current_goal.add_project(project)
        self.current_project = project
        self.current_subproject = None
        self.current_task = None

    def add_subproject(self, entry):
        subproject = Subproject(entry)
        self.current_project.add_item(subproject)
        self.current_subproject = subproject
        self.current_task = None

    def add_task(self, entry):
        task = Task(entry, self.current_goal, self.current_project, self.current_subproject)
        if self.current_subproject:
            self.current_subproject.add_item(task)
        else:
            self.current_project.add_item(task)
        self.current_task = task

    def add_subtask(self, entry):
        subtask = Subtask(entry, self.current_goal, self.current_project, self.current_subproject, self.current_task)
        self.current_task.add_item(subtask)

    def active(self):
        return [goal.active() for goal in self.goals if goal.is_active()]

    def tasklist(self):
        def add(tasklist, goal, project, subproject, items):
            key = (goal, project, subproject)
            if key in tasklist:
                tasklist[key].extend(items)
            else:
                tasklist[key] = items

        tasklist = collections.OrderedDict()
        goals = self.active()
        for goal in goals:
            for project in goal.projects:
                for project_item in project.subitems:
                    if type(project_item) is Subproject:
                        subproject = project_item
                        for subproject_item in subproject.subitems:
                            if type(subproject_item) is Task:
                                task = subproject_item
                                if task.has_subitems():
                                    # task has subtasks, add them
                                    add(tasklist, goal.name, project.name, subproject.name, task.subitems)
                                else:
                                    # task has no subtasks, add it
                                    add(tasklist, goal.name, project.name, subproject.name, [task])
                            if type(subproject_item) is Subtask:
                                subtask = subproject_item
                                add(tasklist, goal.name, project.name, subproject.name, [subtask])
                    if type(project_item) is Task:
                        task = project_item
                        if task.has_subitems():
                            # task has subtasks, add them
                            add(tasklist, goal.name, project.name, '', task.subitems)
                        else:
                            # task has no subtasks, add it
                            add(tasklist, goal.name, project.name, '', [task])
        return tasklist

#-------------------------------------------------------------------------------
def day_idx(day):
    if day == "monday": return 0
    if day == "tuesday": return 1
    if day == "wednesday": return 2
    if day == "thursday": return 3
    if day == "friday": return 4
    if day == "saturday": return 5
    if day == "sunday": return 6
    return 7

def Slot(start, category):
    return {"start": start, "category": category}

class Routine:
    def __init__(self, raw_data):
        data = []
        for line in raw_data:
            entry = RoutineEntry(line)
            if not entry.is_empty():
                data.append(entry)

        self.slots = [
                {"day": "monday", "slots": []},
                {"day": "tuesday", "slots": []},
                {"day": "wednesday", "slots": []},
                {"day": "thursday", "slots": []},
                {"day": "friday", "slots": []},
                {"day": "saturday", "slots": []},
                {"day": "sunday", "slots": []}
            ]

        current_day = None
        for entry in data:
            if entry.is_header():
                pass
            else:
                if entry.is_day():
                    current_day = entry.day
                delta = datetime.timedelta(minutes=30)
                for i in range(entry.duration):
                    start_time = (datetime.datetime.today().replace(hour=entry.start[0], minute=entry.start[1], second=0, microsecond=0) + delta * i).strftime("%H:%M")
                    self.slots[day_idx(current_day)]["slots"].append(Slot(start_time, entry.category))

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def output(self):
        print("ROUTINE:")
        print(self)

    def shift(self):
        self.slots = self.slots[1:] + self.slots[:1]

#-------------------------------------------------------------------------------
class Schedule:
    def __init__(self, todo, routine):

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

        def is_candidate(category, key):
            return category in key

        def build_candidate_list(tasklist, category):
            l = [tasklist[key] for key in tasklist if is_candidate(category, key)]
            return [x for x in roundrobin(*l)]

        # shift routine to start on today
        today = datetime.datetime.isoweekday(datetime.datetime.today()) - 1
        for i in range(today):
            routine.shift()

        tasklist = todo.tasklist()
        categories = set([item for sublist in [[slot["category"] for slot in day["slots"]] for day in routine.slots] for item in sublist])
        candidates = {category: build_candidate_list(tasklist, category) for category in categories}

        self.schedule = copy.copy(routine.slots)
        for day in self.schedule:
            for slot in day["slots"]:
                category = slot["category"]
                category_items = candidates[category]
                if category_items != []:
                    item = category_items[0]
                    slot["item"] = item
                    if item.metadata["duration"] == 1:
                        category_items.remove(item)
                    else:
                        item.metadata["duration"] -= 1

    def __repr__(self):
        return json.dumps(self, indent=2, default=jdefault)

    def output_schedule(self):
        print("SCHEDULE:")

        goals = ["goal"]
        projects = ["project"]
        subprojects = ["subproject"]
        for day in self.schedule:
            for slot in day["slots"]:
                if "item" in slot and type(slot["item"]) in [Task, Subtask]:
                    goals.append(slot["item"].goal)
                    projects.append(slot["item"].project)
                    subprojects.append(slot["item"].subproject)

        maxday = max([len("day")] + [len(day["day"]) for day in self.schedule])
        maxgoal = max([len(goal) for goal in goals])
        maxproject = max([len(project) for project in projects])
        maxsubproject = max([len(subproject) for subproject in subprojects])

        strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | {}"
        print(strfmt.format("day", " time", "goal", "project", "subproject", "task"))

        for day in self.schedule:
            day_name = day["day"]
            for slot in day["slots"]:
                start = slot["start"]
                if "item" in slot and type(slot["item"]) in [Task, Subtask]:
                    item = slot["item"]
                    goal = item.goal
                    project = item.project
                    subproject = item.subproject
                    strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | "
                    if type(item) is Task:
                        task = item.name
                        strfmt += "{}"
                        print(strfmt.format(day_name, start, goal, project, subproject, task))
                    elif type(item) is Subtask:
                        strfmt += "{}:{}"
                        task = item.task
                        subtask = item.name
                        print(strfmt.format(day_name, start, goal, project, subproject, task, subtask))
                    else:
                        print("ERROR: ", item)
                        sys.exit(1)

    def output(self):
        self.output_schedule()

#-------------------------------------------------------------------------------

def output_data(data, todo, routine, schedule):
    schedule.output()

def main():
    filename = "/Users/igaray/Dropbox/textfiles/private/todo.txt"
    parser = OrgParser(filename)
    todo_data, routine_data, notes_data = parser.parse()
    todo = Todo(todo_data)
    routine = Routine(routine_data)
    schedule = Schedule(todo, routine)
    output_data(todo_data, todo, routine, schedule)

if __name__ == "__main__":
    main()
