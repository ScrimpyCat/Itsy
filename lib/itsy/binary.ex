defmodule Itsy.Binary do
    @type endianness :: :big | :little | :native
    @type signedness :: :unsigned | :signed
    @type position :: :high | :low
    @type encodable :: number | bitstring
    @type decode_type :: :integer | :float | :bitstring
    @type decoder :: decode_type | ((bitstring, [encodable]) -> decode_type)
    @type packing_options :: [position: position, into: bitstring, endian: endianness, reverse: boolean]
    @type unpacking_options :: [position: position, count: nil | non_neg_integer, endian: endianness, sign: signedness, reverse: boolean, decoder: decoder]

    @doc """
      Pack a list of integers, floats, or bitstrings into a bitstring.

      The values can be packed into a pre-existing bitstring by setting the `:into`
      option to the desired destination.

      The position in the bitstring the values are packed is determined by the
      `:position` option. A `:high` position will place them in the high-order
      bits, while a `:low` position will place them in the low-order bits. By
      default this is set to `:low`.

      The list can be inserted in the same order or optionally in reverse order,
      when the `:reverse` option is set to true.

      * `[position: :high, reverse: false]` - most significant bit will be based on
      the first number in the list.
      * `[position: :high, reverse: true]` - most significant bit will be based on
      the last number in the list.
      * `[position: :low, reverse: true]` - least significant bit will be based on
      the first number in the list.
      * `[position: :low, reverse: false]` - least significant bit will be based on
      the last number in the list.

      The byte order defaults to big endian, this can be overriden by setting the
      `:endian` option to a kind expected by bitstring parameters.

        iex> Itsy.Binary.pack([0], 2)
        <<0 :: 2>>

        iex> Itsy.Binary.pack([0], 3)
        <<0 :: 3>>

        iex> Itsy.Binary.pack([], 2)
        <<>>

        iex> Itsy.Binary.pack([0, 0, 0, 0], 2)
        <<0 :: 8>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 2)
        <<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 2, position: :high)
        <<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 2, reverse: true)
        <<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 2, position: :high, reverse: true)
        <<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>

        iex> Itsy.Binary.pack([1, 2, 3, 4000], 12, position: :high, reverse: true)
        <<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 3, into: "foo")
        <<"foo", 1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 3, into: "foo", reverse: true)
        <<"foo", 4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 3, into: "foo", position: :high)
        <<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3, "foo">>

        iex> Itsy.Binary.pack([1, 2, 3, 4], 3, into: "foo", position: :high, reverse: true)
        <<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3, "foo">>

        iex> Itsy.Binary.pack([1], 32, endian: :little)
        <<1 :: 8, 0 :: 24>>

        iex> Itsy.Binary.pack([1], 32)
        <<0 :: 24, 1 :: 8>>

        iex> Itsy.Binary.pack([0.5], 32)
        <<63 :: 8, 0 :: 24>>

        iex> Itsy.Binary.pack(["foo", "bar"], 24)
        "foobar"

        iex> Itsy.Binary.pack(["foo", "bar"], 8)
        "fb"

        iex> Itsy.Binary.pack(["foo", "bar"], 32)
        <<"foo", 0 :: 8, "bar", 0 :: 8>>
    """
    @spec pack([encodable], non_neg_integer, packing_options) :: bitstring
    def pack(values, size, opts \\ []) do
        opts = Keyword.merge([position: :low, into: <<>>, endian: :big, reverse: false], opts)

        values = if(opts[:reverse], do: values, else: Enum.reverse(values))
        case opts[:position] do
            :high -> pack(values, size, opts[:into], opts[:endian])
            :low -> <<opts[:into] :: bitstring, pack(values, size, <<>>, opts[:endian]) :: bitstring>>
        end
    end

    defp pad(left, right, padding, size), do: <<left :: bitstring, padding :: size(size), right :: bitstring>>

    defp pack(values, size, bin, :big) do
        padding = 0
        Enum.reduce(values, bin, fn
            value, pack when is_integer(value) -> <<value :: size(size)-big, pack :: bitstring>>
            value, pack when is_float(value) -> <<value :: float-size(size)-big, pack :: bitstring>>
            <<value :: bitstring-size(size), _ :: bitstring>>, pack -> <<value :: bitstring, pack :: bitstring>>
            value, pack -> pad(value, pack, padding, size - bit_size(value))
        end)
    end
    defp pack(values, size, bin, :little) do
        padding = 0
        Enum.reduce(values, bin, fn
            value, pack when is_integer(value) -> <<value :: size(size)-little, pack :: bitstring>>
            value, pack when is_float(value) -> <<value :: float-size(size)-little, pack :: bitstring>>
            <<value :: bitstring-size(size), _ :: bitstring>>, pack -> <<value :: bitstring, pack :: bitstring>>
            value, pack -> pad(value, pack, padding, size - bit_size(value))
        end)
    end
    defp pack(values, size, bin, :native) do
        padding = 0
        Enum.reduce(values, bin, fn
            value, pack when is_integer(value) -> <<value :: size(size)-native, pack :: bitstring>>
            value, pack when is_float(value) -> <<value :: float-size(size)-native, pack :: bitstring>>
            <<value :: bitstring-size(size), _ :: bitstring>>, pack -> <<value :: bitstring, pack :: bitstring>>
            value, pack -> pad(value, pack, padding, size - bit_size(value))
        end)
    end

    @doc """
      Unpack integers, floats, or bitstrings from a bitstring.

      The type of value to unpack is inferred by the `:decoder`. By default this
      option is set to `:integer`, but can be set to any of the other types, or
      a function that will return the type to be used for that value.

      A certain number of values can be unpacked by setting the `:count` option.

      The position in the bitstring the values are packed is determined by the
      `:position` option. A `:high` position will place them in the high-order
      bits, while a `:low` position will place them in the low-order bits. By
      default this is set to `:low`.

      The list can be inserted in the same order or optionally in reverse order,
      when the `:reverse` option is set to true.

      * `[position: :high, reverse: false]` - most significant bit will be used in
      the first number in the list.
      * `[position: :high, reverse: true]` - most significant bit will be used in
      the last number in the list.
      * `[position: :low, reverse: true]` - least significant bit will be used in
      the first number in the list.
      * `[position: :low, reverse: false]` - least significant bit will be used in
      the last number in the list.

      The byte order defaults to big endian, this can be overriden by setting the
      `:endian` option to a kind expected by bitstring parameters.

      The signedness of the integers defaults to unsigned, this can be overriden by
      setting the `:sign` option to a kind expected by bitstring parameters.

        iex> Itsy.Binary.unpack(<<0 :: 2>>, 2)
        [0]

        iex> Itsy.Binary.unpack(<<0 :: 3>>, 3)
        [0]

        iex> Itsy.Binary.unpack(<<>>, 2)
        []

        iex> Itsy.Binary.unpack(<<0 :: 8>>, 2)
        [0, 0, 0, 0]

        iex> Itsy.Binary.unpack(<<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>, 2)
        [1, 2, 3, 0]

        iex> Itsy.Binary.unpack(<<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>, 2, position: :high)
        [1, 2, 3, 0]

        iex> Itsy.Binary.unpack(<<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>, 2, reverse: true)
        [1, 2, 3, 0]

        iex> Itsy.Binary.unpack(<<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>, 2, position: :high, reverse: true)
        [1, 2, 3, 0]

        iex> Itsy.Binary.unpack(<<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>, 12, position: :high, reverse: true)
        [1, 2, 3, 4000]

        iex> Itsy.Binary.unpack(<<"foo", 1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>, 3, count: 4)
        [1, 2, 3, 4]

        iex> Itsy.Binary.unpack(<<"foo", 4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>, 3, count: 4, reverse: true)
        [1, 2, 3, 4]

        iex> Itsy.Binary.unpack(<<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3, "foo">>, 3, count: 4, position: :high)
        [1, 2, 3, 4]

        iex> Itsy.Binary.unpack(<<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3, "foo">>, 3, count: 4, position: :high, reverse: true)
        [1, 2, 3, 4]

        iex> Itsy.Binary.unpack(<<1 :: 8, 0 :: 24>>, 32, endian: :little)
        [1]

        iex> Itsy.Binary.unpack(<<0 :: 24, 1 :: 8>>, 32)
        [1]

        iex> Itsy.Binary.unpack(<<255 :: 8>>, 8, endian: :big, sign: :signed)
        [-1]

        iex> Itsy.Binary.unpack(<<63 :: 8, 0 :: 24>>, 32, decoder: :float)
        [0.5]

        iex> Itsy.Binary.unpack("foobar", 24, decoder: :bitstring)
        ["foo", "bar"]

        iex> Itsy.Binary.unpack(<<"test", 1 :: 32>>, 32, decoder: fn
        ...>     _, [] -> :bitstring
        ...>     _, _ -> :integer
        ...> end)
        ["test", 1]
    """
    @spec unpack(bitstring, non_neg_integer, unpacking_options) :: [encodable]
    def unpack(packed, size, opts \\ []) do
        opts = Keyword.merge([position: :low, count: nil, endian: :big, sign: :unsigned, reverse: false, decoder: :integer], opts)

        values = case opts[:count] do
            nil ->
                count = div(bit_size(packed), size)
                { values, <<>> } = case opts[:position] do
                    :high -> unpack(packed, size, count, opts[:endian], opts[:sign], opts[:decoder])
                    :low -> unpack(chunk(packed, count, size), size, count, opts[:endian], opts[:sign], opts[:decoder])
                end

                values
            count ->
                { values, _ } = case opts[:position] do
                    :high -> unpack(packed, size, count, opts[:endian], opts[:sign], opts[:decoder])
                    :low -> unpack(chunk(packed, count, size), size, count, opts[:endian], opts[:sign], opts[:decoder])
                end

                values
        end

        if(opts[:reverse], do: values, else: Enum.reverse(values))
    end

    defp chunk(packed, count, size) do
        skip_count = bit_size(packed) - (count * size)
        <<_ :: size(skip_count), packed :: bitstring>> = packed
        packed
    end

    defp unpack(packed, size, n, endian, sign, decoder, values \\ [], type \\ nil)
    defp unpack(bin, _, 0, _, _, _, values, _), do: { values, bin }
    defp unpack(packed, size, n, endian, sign, decoder, values, nil) when is_atom(decoder), do: unpack(packed, size, n, endian, sign, decoder, values, decoder)
    defp unpack(packed, size, n, endian, sign, decoder, values, nil), do: unpack(packed, size, n, endian, sign, decoder, values, decoder.(packed, values))
    defp unpack(packed, size, n, endian = :big, sign = :unsigned, decoder, values, :integer) do
        <<value :: size(size)-big-unsigned, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :big, sign = :signed, decoder, values, :integer) do
        <<value :: size(size)-big-signed, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :little, sign = :unsigned, decoder, values, :integer) do
        <<value :: size(size)-little-unsigned, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :little, sign = :signed, decoder, values, :integer) do
        <<value :: size(size)-little-signed, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :native, sign = :unsigned, decoder, values, :integer) do
        <<value :: size(size)-native-unsigned, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :native, sign = :signed, decoder, values, :integer) do
        <<value :: size(size)-native-signed, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :big, sign, decoder, values, :float) do
        <<value :: float-size(size)-big, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :little, sign, decoder, values, :float) do
        <<value :: float-size(size)-little, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian = :native, sign, decoder, values, :float) do
        <<value :: float-size(size)-native, packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
    defp unpack(packed, size, n, endian, sign, decoder, values, :bitstring) do
        <<value :: bitstring-size(size), packed :: bitstring>> = packed
        unpack(packed, size, n - 1, endian, sign, decoder, [value|values], nil)
    end
end
