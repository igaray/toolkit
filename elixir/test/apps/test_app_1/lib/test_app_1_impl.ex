defmodule TestApp1.Job.Area do
    def new() do
      :ok
    end
end

defimpl TestApp1.Protocol, for: TestApp1.Job.Area do
    def execute(_job, _trigger) do
       :ok
    end
end
