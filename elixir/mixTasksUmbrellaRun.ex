defmodule Mix.Tasks.Umbrella.Run do
    use Mix.Task

    @shortdoc "Runs a task for a single app in an umbrella"

    @moduledoc """
    Runs a task for a single app in an umbrella.

      mix umbrella.run TASK --apps APP1,APP2,APP3 ...

    This task runs from the root of an umbrella project and runs a specified
    task only for the listed apps.

    The `--apps` option is mandatory and specifies for which app the given
    task will be run. This is a comma-separated list of app names, with no
    spaces around commas. A single app may be specified.

    All remaining arguments are passed as-is to the given task.

    ## Command line options

    * `--apps` - the comma-separated list of apps for which the task will be run
    """

    @switches [apps: :keep]
    @recursive true

    def run(args) do
        if (length args) == 0 do
            Mix.raise "Task not provided"
        end

        {opts, _files} = OptionParser.parse! args
        config = Mix.Project.config
        app = Atom.to_string config[:app]
        apps = opts[:apps] |> String.split(",")

        if Enum.member?(apps, app) do
            [task, "--apps", _ | task_args] = args
            Mix.Task.run(task, task_args)
        end
    end
end
