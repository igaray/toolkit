defmodule TestApp1 do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      # Starts a worker by calling: TestApp1.Worker.start_link(arg1, arg2, arg3)
      # worker(TestApp1.Worker, [arg1, arg2, arg3]),
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: TestApp1.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def test do
    IO.puts "Test1"

    try do
	:ok = g()
    rescue
        e in MatchError -> 
            %MatchError{term: {:error, reason}} = e
            IO.puts "A match error occured: #{inspect reason}"
	f -> 
            IO.puts "An unexpected error occurred: #{inspect f}"
    end
  end

  defp g do
    {:error, "reason"}
    1 / 0
  end
end
