import datetime
import os
import pprint
import string
import sys

#-------------------------------------------------------------------------------
class Entry:
    pass

def is_goal(entry):
    return entry[0] != ''

def is_project(entry):
    return entry[1] != ''

def is_subproject(entry):
    return entry[2] != ''

def is_task(entry):
    return entry[3] != ''

def is_subtask(entry):
    return entry[4] != ''

def is_empty(entry):
    return (entry == []) or (entry == ['']) or (entry == ['', '', '', '', '', ''])

def is_routine(entry):
    return entry[0] == 'routine'

def is_notes(entry):
    return entry[0] == 'notes:'

def is_active(entry):
    return entry[0] == '+'

def is_day(entry):
    day = entry[0]
    return (day == 'monday') or (day == 'tuesday') or (day == 'wednesday') or (day == 'thursday') or (day == 'friday') or (day == 'saturday') or (day == 'sunday')
#-------------------------------------------------------------------------------
class Metadata:
    pass

def parse_metadata(entry):
    metadata = {}
    metadata_raw = entry[5]
    metadata_fields = [ field.split(':') for field in metadata_raw.split(',')]
    if metadata_raw != '': 
        for field in metadata_fields:
            metadata[field[0]] = field[1]
        metadata["duration"] = int(metadata["duration"])
    return metadata

#-------------------------------------------------------------------------------
class Subtask:
    pass

def new_subtask(title, duration, current):
    return {"goal": current["goal"], "project": current["project"], "subproject": current["subproject"], "task": current["task"], "subtask": title, "duration": duration}

def parse_subtask(entry):
    title = entry[4][2:]
    return title

#-------------------------------------------------------------------------------
class Task:
    pass

def new_task(title, duration, current):
    return {"goal": current["goal"], "project": current["project"], "subproject": current["subproject"], "task": title, "subtasks": [], "duration": duration}

def parse_task(entry):
    title = entry[3][2:]
    return title

#-------------------------------------------------------------------------------
class Subproject:
    pass

def new_subproject(title, active):
    return {"subproject": title, "active": active, "tasks": []}

def parse_subproject(entry):
    title = entry[2][2:]
    active = is_active(entry[2])
    return (title, active)

#-------------------------------------------------------------------------------
class Project:
    pass

def new_project(title, active):
    subproject = new_subproject("default", active)
    return {"project": title, "active": active, "subprojects": [subproject]}

def parse_project(entry):
    title = entry[1][2:]
    active = is_active(entry[1])
    return (title, active)

#-------------------------------------------------------------------------------
class Goal:
    pass

def new_goal(title, active):
    return {"goal": title, "active": active, "projects": []}

def parse_goal(entry):
    title = entry[0][2:]
    active = is_active(entry[0])
    return (title, active)

#-------------------------------------------------------------------------------
class Todo:
    pass

def get_task(current, todo):
    subproject = get_subproject(current, todo)
    tasks = [x for x in subproject["tasks"] if x["task"] == current["task"]]
    return tasks[0]

def get_subproject(current, todo):
    project = get_project(current, todo)
    subprojects = [x for x in project["subprojects"] if x["subproject"] == current["subproject"]]
    return subprojects[0]

def get_project(current, todo):
    goal = get_goal(current, todo)
    projects = [x for x in goal["projects"] if x["project"] == current["project"]]
    return projects[0]

def get_goal(current, todo):
    goals = [x for x in todo if x["goal"] == current["goal"]]
    return goals[0]

def add_subtask(current, todo, entry):
    title = parse_subtask(entry)
    metadata = parse_metadata(entry)
    task = get_task(current, todo)
    if "duration" in metadata:
        duration = metadata["duration"]
    else:
        duration = 1
    subtask = new_subtask(title, duration, current)
    task["subtasks"].append(subtask)

def add_task(current, todo, entry):
    title = parse_task(entry)
    metadata = parse_metadata(entry)
    if "duration" in metadata:
        duration = metadata["duration"]
    else:
        duration = 1
    subproject = get_subproject(current, todo)
    task = new_task(title, duration, current)
    subproject["tasks"].append(task)
    current["task"] = title

def add_subproject(current, todo, entry):
    title, active = parse_subproject(entry)
    project = get_project(current, todo)
    subproject = new_subproject(title, active)
    project["subprojects"].append(subproject)
    current["subproject"] = title
    current["task"] = None

def add_project(current, todo, entry):
    title, active = parse_project(entry)
    goal = get_goal(current, todo)
    project = new_project(title, active)
    goal["projects"].append(project)
    current["project"] = title
    current["subproject"] = "default"
    current["task"] = None

def add_goal(current, todo, entry):
    title, active = parse_goal(entry)
    goal = new_goal(title, active)
    todo.append(goal)
    current["goal"] = title
    current["project"] = None
    current["subproject"] = "default"
    current["task"] = None

def parse_todo(data):
    entries = 0
    todo = []
    current = {}
    current["goal"] = None
    current["project"] = None
    current["subproject"] = "default"
    current["task"] = None
    for entry in data:
        if is_routine(entry):
            break
        elif is_goal(entry): 
            add_goal(current, todo, entry)
        elif is_project(entry): 
            add_project(current, todo, entry)
        elif is_subproject(entry): 
            add_subproject(current, todo, entry)
        elif is_task(entry): 
            add_task(current, todo, entry)
        elif is_subtask(entry): 
            add_subtask(current, todo, entry)
        else:
            print("ERROR entry: {}".format(entry))
        entries += 1
    data = data[entries:]
    return (todo, data)

#-------------------------------------------------------------------------------
class Routine:
    pass

def new_slot(day, start, category):
    return {"day": day, "start": start, "category": category}

def add_slots(day, routine, entry):
    delta = datetime.timedelta(minutes=30)
    start = [int(x) for x in entry[1].split(':')]
    duration = int(entry[2])
    category = entry[3]
    for i in range(duration):
        start_time = (datetime.datetime.today().replace(hour=start[0], minute=start[1], second=0, microsecond=0) + delta * i).strftime("%H:%M")
        slot = new_slot(day, start_time, category)
        routine.append(slot)

def parse_routine(data):
    entries = 0
    routine = []
    current_day = None
    for entry in data[1:]:
        if is_notes(entry):
            break
        elif is_day(entry):
            current_day = entry[0]
        add_slots(current_day, routine, entry)
    return (routine, data)

#-------------------------------------------------------------------------------
class Schedule:
    pass

def get_tasks(todo):
    tasks = []
    for goal in todo:
        for project in goal["projects"]:
            for subproject in project["subprojects"]:
                for task in subproject["tasks"]:
                    if task["subtasks"] != []:
                        tasks.extend(task["subtasks"])
                    else:
                        tasks.append(task)
    return tasks

def active_subprojects(project):
    project["subprojects"] = [subproject for subproject in project["subprojects"] if subproject["active"] == True]
    return project

def active_projects(goal):
    goal["projects"] = [active_subprojects(project) for project in goal["projects"] if project["active"] == True]
    return goal

def active_goals(todo):
    return [active_projects(goal) for goal in todo if goal["active"] == True]

def schedule_week(tasks, routine):
    schedule = routine
    goals = active_goals(tasks)
    tasklist = get_tasks(goals)

    # shift routine to start on today
    today = datetime.datetime.isoweekday(datetime.datetime.today())

    for slot in routine:
        for task in tasklist:
            pp = pprint.PrettyPrinter(indent=2, width=280)
            if (slot["category"] == task["goal"]) or (slot["category"] == task["project"]) or (slot["category"] == task["subproject"]):
                slot["item"] = task
                if task["duration"] == 1:
                    tasklist.remove(task)
                else:
                    task["duration"] -= 1
                break
    return schedule

#-------------------------------------------------------------------------------
def output(data, todo, routine, schedule):
    pp = pprint.PrettyPrinter(indent=2, width=280)
    #print("TODO:")
    #pp.pprint(todo)
    #print("ROUTINE:")
    #pp.pprint(routine)
    #print("SCHEDULE:")
    #pp.pprint(schedule)

    maxday = max([len("day")] + [len(slot["day"]) for slot in schedule])
    maxgoal = max([len("goal")] + [len(slot["item"]["goal"]) for slot in schedule if "item" in slot])
    maxproject = max([len("project")] + [len(slot["item"]["project"]) for slot in schedule if "item" in slot])
    maxsubproject = max([len("subproject")] + [len(slot["item"]["subproject"]) for slot in schedule if "item" in slot])

    strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | {}"
    print(strfmt.format("day", " time", "goal", "project", "subproject", "task"))
    for slot in schedule:
        day = slot["day"]
        start = slot["start"]
        if "item" in slot:
            item = slot["item"]
            goal = item["goal"]
            project = item["project"]
            subproject = item["subproject"]
            task = item["task"]
            strfmt = "{: >" + str(maxday) + "} {} | {: >" + str(maxgoal) + "} | {: >" + str(maxproject) + "} | {: >" + str(maxsubproject) + "} | "
            if subproject == "default":
                subproject = ""
            if "subtask" in item:
                strfmt += "{}:{}"
                subtask = item["subtask"]
                print(strfmt.format(day, start, goal, project, subproject, task, subtask))
            else:
                strfmt += "{}"
                print(strfmt.format(day, start, goal, project, subproject, task))

def read():
    with open("/Users/igaray/Dropbox/textfiles/private/todo.txt", "r") as todofile:
        filecontents = []
        for line in todofile:
            filecontents.append([field.strip(" <>") for field in line[:-1].split("|")])
        filecontents = [item for item in filecontents[1:-1] if not is_empty(item)]
    return filecontents

def main():
    data = read()
    tasks, data = parse_todo(data)
    routine, data = parse_routine(data)
    schedule = schedule_week(tasks, routine)
    output(data, tasks, routine, schedule)

if __name__ == "__main__":
    main()
