import datetime
import os
import pprint
import string
import sys

#-------------------------------------------------------------------------------
class RoutineEntry:
    
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
        d = {
                "fields": self.fields,
                "day": self.day,
                "start": self.start,
                "duration": self.duration,
                "category": self.category
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

    def is_empty(self):
        return (self.fields == []) or (self.fields == ['']) or (self.fields == ['', '', '', ''])

    def is_header(self):
        return self.fields[0] == 'routine'

    def is_day(self):
        return self.fields[0] in ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday']

#-------------------------------------------------------------------------------
class TodoEntry:

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
                    else:
                        self.metadata["active"] = False
                    field = field[2:]
                    break

            self.goal = self.fields[0][2:]
            self.project = self.fields[1][2:]
            self.subproject = self.fields[2][2:]
            self.task = self.fields[3][2:]
            self.subtask = self.fields[4][2:]

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
        d = {
                "fields": self.fields,
                "goal": self.goal,
                "project": self.project,
                "subproject": self.subproject,
                "task": self.task,
                "subtask": self.subtask,
                "metadata": self.metadata
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

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
        self.active = entry.metadata["active"]
        self.projects = []

    def __repr__(self):
        d = {
                "class": "goal",
                "name": self.name, 
                "active": self.active, 
                "projects": self.projects
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

    def active_projects(self):
        self.projects = [project.active_subprojects() for project in self.projects if project.active == True]
        return self

#-------------------------------------------------------------------------------
class Project:

    def __init__(self, entry):
        self.name = entry.project
        self.active = entry.metadata["active"]
        self.subprojects = [Subproject(entry)]

    def __repr__(self):
        d = {
                "class": "project",
                "name": self.name, 
                "active": self.active, 
                "subprojects": self.subprojects
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

    def active_subprojects(self):
        self.subprojects = [subproject for subproject in self.subprojects if subproject.active == True]
        return self

#-------------------------------------------------------------------------------
class Subproject:

    def __init__(self, entry):
        if entry.subproject == '':
            self.name = "default"
        else:
            self.name = entry.subproject
        self.active = entry.metadata["active"]
        self.tasks = []

    def __repr__(self):
        d = {
                "class": "subproject",
                "name": self.name, 
                "active": self.active, 
                "tasks": self.tasks
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

#-------------------------------------------------------------------------------
class Task:

    def __init__(self, entry, goal, project, subproject):
        self.name = entry.task
        self.goal = goal
        self.project = project
        self.subproject = subproject
        self.subtasks = []
        self.duration = entry.metadata["duration"]

    def __repr__(self):
        d = {
                "class": "task",
                "name": self.name, 
                "goal": self.goal, 
                "project": self.project, 
                "subproject": self.subproject, 
                "subtasks": self.subtasks, 
                "duration": self.duration
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

#-------------------------------------------------------------------------------
class Subtask:

    def __init__(self, entry, goal, project, subproject, task):
        self.name = entry.subtask
        self.goal = goal
        self.project = project
        self.subproject = project
        self.task = task
        self.duration = entry.metadata["duration"]

    def __repr__(self):
        d = {
                "class": "subtask", 
                "name": self.name, 
                "goal": self.goal, 
                "project": self.project, 
                "subproject": self.subproject, 
                "task": self.task, 
                "duration": self.duration
            }
        s = str(d)
        return s

    def __str__(self):
        return self.__repr__()

#-------------------------------------------------------------------------------
class Todo:
    
    def __init__(self, raw_data):

        data = []
        for line in raw_data:
            entry = TodoEntry(line)
            if not entry.is_empty():
                data.append(entry)

        self.todo = []

        self.current = {}
        self.current["goal"] = None
        self.current["project"] = None
        self.current["subproject"] = "default"
        self.current["task"] = None

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
        return str(self.todo)

    def __str__(self):
        return str(self.todo)

    def get_goal(self):
        for goal in self.todo:
            if goal.name == self.current["goal"]:
                return goal

    def get_project(self):
        goal = self.get_goal()
        projects = [project for project in goal.projects if project.name == self.current["project"]]
        return projects[0]

    def get_subproject(self):
        project = self.get_project()
        subprojects = [subproject for subproject in project.subprojects if subproject.name == self.current["subproject"]]
        return subprojects[0]

    def get_task(self):
        subproject = self.get_subproject()
        tasks = [task for task in subproject.tasks if task.name == self.current["task"]]
        return tasks[0]

    def active_items(self):
        return [goal.active_projects() for goal in self.todo if goal.active == True]

    def add_goal(self, entry):
        goal = Goal(entry)
        self.todo.append(goal)
        self.current["goal"] = goal.name
        self.current["project"] = None
        self.current["subproject"] = "default"
        self.current["task"] = None

    def add_project(self, entry):
        project = Project(entry)
        goal = self.get_goal()
        goal.projects.append(project)
        self.current["project"] = project.name
        self.current["subproject"] = "default"
        self.current["task"] = None

    def add_subproject(self, entry):
        subproject = Subproject(entry)
        project = self.get_project()
        project.subprojects.append(subproject)
        self.current["subproject"] = subproject.name
        self.current["task"] = None

    def add_task(self, entry):
        task = Task(entry, self.current["goal"], self.current["project"], self.current["subproject"])
        subproject = self.get_subproject()
        subproject.tasks.append(task)
        self.current["task"] = task.name

    def add_subtask(self, entry):
        subtask = Subtask(entry, self.current["goal"], self.current["project"], self.current["subproject"], self.current["task"])
        task = self.get_task()
        task.subtasks.append(subtask)

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

def Slot(day, start, category):
    return {"day": day, "start": start, "category": category}

class Routine:

    def __init__(self, raw_data):
        data = []
        for line in raw_data:
            entry = RoutineEntry(line)
            if not entry.is_empty():
                data.append(entry)

        self.slots = [
                {"day": "monday", "pomos": []}, 
                {"day": "tuesday", "pomos": []}, 
                {"day": "wednesday", "pomos": []}, 
                {"day": "thursday", "pomos": []}, 
                {"day": "friday", "pomos": []}, 
                {"day": "saturday", "pomos": []}, 
                {"day": "sunday", "pomos": []}
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
                    self.slots[day_idx(current_day)]["pomos"].append(Slot(current_day, start_time, entry.category))

    def __repr__(self):
        return str(self.slots)

    def __str__(self):
        return self.__repr()

    def shift(self):
        self.slots = self.slots[1:] + self.slots[:1]

#-------------------------------------------------------------------------------
class Schedule:

    def __init__(self, todo, routine):
        self.schedule = []

        goals = todo.active_items()
        tasklist = self.get_tasks(goals)

        # shift routine to start on today
        today = datetime.datetime.isoweekday(datetime.datetime.today()) - 1
        for i in range(today):
            routine.shift()

        for day in routine.slots:
            for slot in day["pomos"]:
                for task in tasklist:
                    if slot["category"] in [task.goal, task.project, task.subproject]:
                        self.schedule.append(slot)
                        slot["item"] = task
                        if task.duration == 1:
                            tasklist.remove(task)
                        else:
                            task.duration -= 1
                        break

    def get_tasks(self, todo):
        tasks = []
        for goal in todo:
            for project in goal.projects:
                for subproject in project.subprojects:
                    for task in subproject.tasks:
                        if task.subtasks != []:
                            tasks.extend(task.subtasks)
                        else:
                            tasks.append(task)
        return tasks

    def output(self):
        maxday = max([len("day")] + [len(slot["day"]) for slot in self.schedule])
        maxgoal = max([len("goal")] + [len(slot["item"].goal) for slot in self.schedule if "item" in slot])
        maxproject = max([len("project")] + [len(slot["item"].project) for slot in self.schedule if "item" in slot])
        maxsubproject = max([len("subproject")] + [len(slot["item"].subproject) for slot in self.schedule if "item" in slot])

        strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | {}"
        print(strfmt.format("day", " time", "goal", "project", "subproject", "task"))
        for slot in self.schedule:
            day = slot["day"]
            start = slot["start"]
            if "item" in slot:
                item = slot["item"]
                goal = item.goal
                project = item.project
                subproject = item.subproject
                task = item.name
                strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | "
                if subproject == "default":
                    subproject = ""
                if type(item) is Subtask:
                    strfmt += "{}:{}"
                    subtask = item.name
                    print(strfmt.format(day, start, goal, project, subproject, task, subtask))
                else:
                    strfmt += "{}"
                    print(strfmt.format(day, start, goal, project, subproject, task))

#-------------------------------------------------------------------------------
def read_data():
    with open("/Users/igaray/Dropbox/textfiles/private/todo.txt", "r") as todofile:
        data = [[], [], []]
        i = 0
        for line in todofile:
            if line == "%%\n":
                i += 1
            else:
                data[i].append(line)
    return (data[0], data[1], data[2])

def output_data(data, todo, routine, schedule):
    pp = pprint.PrettyPrinter(indent=2, width=280)
    #print("TODO:")
    #pp.pprint(todo)
    #print("ROUTINE:")
    #pp.pprint(routine)
    print("SCHEDULE:")
    schedule.output()

def main():
    todo_data, routine_data, notes_data = read_data()
    todo = Todo(todo_data)
    routine = Routine(routine_data)
    schedule = Schedule(todo, routine)
    output_data(todo_data, todo, routine, schedule)

if __name__ == "__main__":
    main()
