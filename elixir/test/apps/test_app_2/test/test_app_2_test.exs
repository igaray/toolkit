defmodule TestApp2Test do
  use ExUnit.Case
  doctest TestApp2

  test "the truth" do
    assert 1 + 1 == 2
    TestApp2.test
  end
end
