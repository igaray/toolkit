defmodule TestApp1Test do
  use ExUnit.Case
  doctest TestApp1

  test "the truth" do
    assert 1 + 1 == 2
    TestApp1.test
  end
end
