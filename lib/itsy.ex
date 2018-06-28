defmodule Itsy do
    @moduledoc false

    defmodule PowerOf2Error do
        defexception [:message, :value]

        def exception(option), do: %PowerOf2Error{ message: "value #{option[:value]} is not a power of 2", value: option[:value] }
    end
end
