defmodule Itsy.Binary do
    @type endianness :: :big | :little | :native
    @type sign :: :unsigned | :signed

    @doc """
      Pack a list of integers in reverse order into the high-order bits of a
      bitstring.

      The most significant bit will be based on the last number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

        iex> Itsy.Binary.l_pack_reverse([0], 2)
        <<0 :: 2>>

        iex> Itsy.Binary.l_pack_reverse([0], 3)
        <<0 :: 3>>

        iex> Itsy.Binary.l_pack_reverse([], 2)
        <<>>

        iex> Itsy.Binary.l_pack_reverse([0, 0, 0, 0], 2)
        <<0 :: 8>>

        iex> Itsy.Binary.l_pack_reverse([1, 2, 3, 4], 2)
        <<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>

        iex> Itsy.Binary.l_pack_reverse([1, 2, 3, 4], 3, <<>>)
        <<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>

        iex> Itsy.Binary.l_pack_reverse([1, 2, 3, 4000], 12)
        <<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>

        iex> Itsy.Binary.l_pack_reverse([1, 2, 3, 4], 3, "foo")
        <<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3, "foo">>

        iex> Itsy.Binary.l_pack_reverse([1], 32, <<>>, :little)
        <<1 :: 8, 0 :: 24>>

        iex> Itsy.Binary.l_pack_reverse([1], 32, <<>>)
        <<0 :: 24, 1 :: 8>>
    """
    @spec l_pack_reverse([integer], non_neg_integer, bitstring, endianness) :: bitstring
    def l_pack_reverse(values, size, bin \\ <<>>, endian \\ :big)
    def l_pack_reverse(values, size, bin, :big) do
        Enum.reduce(values, bin, fn value, pack ->
            <<value :: size(size)-big, pack :: bitstring>>
        end)
    end
    def l_pack_reverse(values, size, bin, :little) do
        Enum.reduce(values, bin, fn value, pack ->
            <<value :: size(size)-little, pack :: bitstring>>
        end)
    end
    def l_pack_reverse(values, size, bin, :native) do
        Enum.reduce(values, bin, fn value, pack ->
            <<value :: size(size)-native, pack :: bitstring>>
        end)
    end

    @doc """
      Pack a list of integers into the high-order bits of a bitstring.

      The most significant bit will be based on the first number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

        iex> Itsy.Binary.l_pack([0], 2)
        <<0 :: 2>>

        iex> Itsy.Binary.l_pack([0], 3)
        <<0 :: 3>>

        iex> Itsy.Binary.l_pack([], 2)
        <<>>

        iex> Itsy.Binary.l_pack([0, 0, 0, 0], 2)
        <<0 :: 8>>

        iex> Itsy.Binary.l_pack([1, 2, 3, 4], 2)
        <<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>

        iex> Itsy.Binary.l_pack([1, 2, 3, 4], 3, <<>>)
        <<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>

        iex> Itsy.Binary.l_pack([1, 2, 3, 4000], 12)
        <<1 :: 12, 2 :: 12, 3 :: 12, 4000 :: 12>>

        iex> Itsy.Binary.l_pack([1, 2, 3, 4], 3, "foo")
        <<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3, "foo">>

        iex> Itsy.Binary.l_pack([1], 32, <<>>, :little)
        <<1 :: 8, 0 :: 24>>

        iex> Itsy.Binary.l_pack([1], 32, <<>>)
        <<0 :: 24, 1 :: 8>>
    """
    @spec l_pack([integer], non_neg_integer, bitstring, endianness) :: bitstring
    def l_pack(values, size, bin \\ <<>>, endian \\ :big), do: l_pack_reverse(Enum.reverse(values), size, bin, endian)

    @doc """
      Pack a list of integers into the low-order bits of a bitstring.

      The least significant bit will be based on the last number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

        iex> Itsy.Binary.r_pack([0], 2)
        <<0 :: 2>>

        iex> Itsy.Binary.r_pack([0], 3)
        <<0 :: 3>>

        iex> Itsy.Binary.r_pack([], 2)
        <<>>

        iex> Itsy.Binary.r_pack([0, 0, 0, 0], 2)
        <<0 :: 8>>

        iex> Itsy.Binary.r_pack([1, 2, 3, 4], 2)
        <<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>

        iex> Itsy.Binary.r_pack([1, 2, 3, 4], 3, <<>>)
        <<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>

        iex> Itsy.Binary.r_pack([1, 2, 3, 4000], 12)
        <<1 :: 12, 2 :: 12, 3 :: 12, 4000 :: 12>>

        iex> Itsy.Binary.r_pack([1, 2, 3, 4], 3, "foo")
        <<"foo", 1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>

        iex> Itsy.Binary.r_pack([1], 32, <<>>, :little)
        <<1 :: 8, 0 :: 24>>

        iex> Itsy.Binary.r_pack([1], 32, <<>>)
        <<0 :: 24, 1 :: 8>>
    """
    @spec r_pack([integer], non_neg_integer, bitstring, endianness) :: bitstring
    def r_pack(values, size, bin \\ <<>>, endian \\ :big), do: <<bin :: bitstring, l_pack(values, size, <<>>, endian) :: bitstring>>

    @doc """
      Pack a list of integers into the low-order bits of a bitstring.

      The least significant bit will be based on the first number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

        iex> Itsy.Binary.r_pack_reverse([0], 2)
        <<0 :: 2>>

        iex> Itsy.Binary.r_pack_reverse([0], 3)
        <<0 :: 3>>

        iex> Itsy.Binary.r_pack_reverse([], 2)
        <<>>

        iex> Itsy.Binary.r_pack_reverse([0, 0, 0, 0], 2)
        <<0 :: 8>>

        iex> Itsy.Binary.r_pack_reverse([1, 2, 3, 4], 2)
        <<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>

        iex> Itsy.Binary.r_pack_reverse([1, 2, 3, 4], 3, <<>>)
        <<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>

        iex> Itsy.Binary.r_pack_reverse([1, 2, 3, 4000], 12)
        <<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>

        iex> Itsy.Binary.r_pack_reverse([1, 2, 3, 4], 3, "foo")
        <<"foo", 4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>

        iex> Itsy.Binary.r_pack_reverse([1], 32, <<>>, :little)
        <<1 :: 8, 0 :: 24>>

        iex> Itsy.Binary.r_pack_reverse([1], 32, <<>>)
        <<0 :: 24, 1 :: 8>>
    """
    @spec r_pack_reverse([integer], non_neg_integer, bitstring, endianness) :: bitstring
    def r_pack_reverse(values, size, bin \\ <<>>, endian \\ :big), do: <<bin :: bitstring, l_pack_reverse(values, size, <<>>, endian) :: bitstring>>

    @doc """
      Unpack integers in reverse order from the high-order bits of a bitstring.

      The most significant bit will be used in the last number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

      Integer signedness defaults to unsigned, this can be overriden by passing
      in another sign of any kind expected by bitstring parameters.

        iex> Itsy.Binary.l_unpack_reverse(<<0 :: 2>>, 2)
        [0]

        iex> Itsy.Binary.l_unpack_reverse(<<0 :: 3>>, 3)
        [0]

        iex> Itsy.Binary.l_unpack_reverse(<<>>, 2)
        []

        iex> Itsy.Binary.l_unpack_reverse(<<0 :: 8>>, 2)
        [0, 0, 0, 0]

        iex> Itsy.Binary.l_unpack_reverse(<<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>, 2)
        [1, 2, 3, 0]

        iex> Itsy.Binary.l_unpack_reverse(<<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>, 12)
        [1, 2, 3, 4000]

        iex> Itsy.Binary.l_unpack_reverse(<<4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3, "foo">>, 3, 4)
        [1, 2, 3, 4]

        iex> Itsy.Binary.l_unpack_reverse(<<1 :: 8, 0 :: 24>>, 32, nil, :little)
        [1]

        iex> Itsy.Binary.l_unpack_reverse(<<0 :: 24, 1 :: 8>>, 32)
        [1]

        iex> Itsy.Binary.l_unpack_reverse(<<255 :: 8>>, 8, nil, :big, :signed)
        [-1]
    """
    @spec l_unpack_reverse(bitstring, non_neg_integer, pos_integer | nil, endianness, sign) :: [integer]
    def l_unpack_reverse(packed, size, count \\ nil, endian \\ :big, sign \\ :unsigned)
    def l_unpack_reverse(packed, size, nil, endian, sign) do
        { values, <<>> } = l_unpack_reverse(packed, size, div(bit_size(packed), size), endian, sign, [])
        values
    end
    def l_unpack_reverse(packed, size, count, endian, sign) do
        { values, _ } = l_unpack_reverse(packed, size, count, endian, sign, [])
        values
    end

    defp l_unpack_reverse(bin, _, 0, _, _, values), do: { values, bin }
    defp l_unpack_reverse(packed, size, n, endian = :big, sign = :unsigned, values) do
        <<value :: size(size)-big-unsigned, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end
    defp l_unpack_reverse(packed, size, n, endian = :big, sign = :signed, values) do
        <<value :: size(size)-big-signed, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end
    defp l_unpack_reverse(packed, size, n, endian = :little, sign = :unsigned, values) do
        <<value :: size(size)-little-unsigned, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end
    defp l_unpack_reverse(packed, size, n, endian = :little, sign = :signed, values) do
        <<value :: size(size)-little-signed, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end
    defp l_unpack_reverse(packed, size, n, endian = :native, sign = :unsigned, values) do
        <<value :: size(size)-native-unsigned, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end
    defp l_unpack_reverse(packed, size, n, endian = :native, sign = :signed, values) do
        <<value :: size(size)-native-signed, packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, n - 1, endian, sign, [value|values])
    end

    @doc """
      Unpack integers from the high-order bits of a bitstring.

      The most significant bit will be used in the first number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

      Integer signedness defaults to unsigned, this can be overriden by passing
      in another sign of any kind expected by bitstring parameters.

        iex> Itsy.Binary.l_unpack(<<0 :: 2>>, 2)
        [0]

        iex> Itsy.Binary.l_unpack(<<0 :: 3>>, 3)
        [0]

        iex> Itsy.Binary.l_unpack(<<>>, 2)
        []

        iex> Itsy.Binary.l_unpack(<<0 :: 8>>, 2)
        [0, 0, 0, 0]

        iex> Itsy.Binary.l_unpack(<<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>, 2)
        [1, 2, 3, 0]

        iex> Itsy.Binary.l_unpack(<<1 :: 12, 2 :: 12, 3 :: 12, 4000 :: 12>>, 12)
        [1, 2, 3, 4000]

        iex> Itsy.Binary.l_unpack(<<1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3, "foo">>, 3, 4)
        [1, 2, 3, 4]

        iex> Itsy.Binary.l_unpack(<<1 :: 8, 0 :: 24>>, 32, nil, :little)
        [1]

        iex> Itsy.Binary.l_unpack(<<0 :: 24, 1 :: 8>>, 32)
        [1]

        iex> Itsy.Binary.l_unpack(<<255 :: 8>>, 8, nil, :big, :signed)
        [-1]
    """
    @spec l_unpack(bitstring, non_neg_integer, pos_integer | nil, endianness, sign) :: [integer]
    def l_unpack(packed, size, count \\ nil, endian \\ :big, sign \\ :unsigned), do: Enum.reverse(l_unpack_reverse(packed, size, count, endian, sign))

    @doc """
      Unpack integers from the low-order bits of a bitstring.

      The least significant bit will be used in the last number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

      Integer signedness defaults to unsigned, this can be overriden by passing
      in another sign of any kind expected by bitstring parameters.

        iex> Itsy.Binary.r_unpack(<<0 :: 2>>, 2)
        [0]

        iex> Itsy.Binary.r_unpack(<<0 :: 3>>, 3)
        [0]

        iex> Itsy.Binary.r_unpack(<<>>, 2)
        []

        iex> Itsy.Binary.r_unpack(<<0 :: 8>>, 2)
        [0, 0, 0, 0]

        iex> Itsy.Binary.r_unpack(<<1 :: 2, 2 :: 2, 3 :: 2, 0 :: 2>>, 2)
        [1, 2, 3, 0]

        iex> Itsy.Binary.r_unpack(<<1 :: 12, 2 :: 12, 3 :: 12, 4000 :: 12>>, 12)
        [1, 2, 3, 4000]

        iex> Itsy.Binary.r_unpack(<<"foo", 1 :: 3, 2 :: 3, 3 :: 3, 4 :: 3>>, 3, 4)
        [1, 2, 3, 4]

        iex> Itsy.Binary.r_unpack(<<1 :: 8, 0 :: 24>>, 32, nil, :little)
        [1]

        iex> Itsy.Binary.r_unpack(<<0 :: 24, 1 :: 8>>, 32)
        [1]

        iex> Itsy.Binary.r_unpack(<<255 :: 8>>, 8, nil, :big, :signed)
        [-1]
    """
    @spec r_unpack(bitstring, non_neg_integer, pos_integer | nil, endianness, sign) :: [integer]
    def r_unpack(packed, size, count \\ nil, endian \\ :big, sign \\ :unsigned)
    def r_unpack(packed, size, nil, endian, sign), do: l_unpack(packed, size, nil, endian, sign)
    def r_unpack(packed, size, count, endian, sign) do
        skip_count = bit_size(packed) - (count * size)
        <<_ :: size(skip_count), packed :: bitstring>> = packed
        l_unpack(packed, size, nil, endian, sign)
    end

    @doc """
      Unpack integers in reverse order from the high-order bits of a bitstring.

      The most significant bit will be used in the last number in the list.

      The byte order defaults to big endian, this can be overriden by passing
      in another endian of a kind expected by bitstring parameters.

      Integer signedness defaults to unsigned, this can be overriden by passing
      in another sign of any kind expected by bitstring parameters.

        iex> Itsy.Binary.r_unpack_reverse(<<0 :: 2>>, 2)
        [0]

        iex> Itsy.Binary.r_unpack_reverse(<<0 :: 3>>, 3)
        [0]

        iex> Itsy.Binary.r_unpack_reverse(<<>>, 2)
        []

        iex> Itsy.Binary.r_unpack_reverse(<<0 :: 8>>, 2)
        [0, 0, 0, 0]

        iex> Itsy.Binary.r_unpack_reverse(<<0 :: 2, 3 :: 2, 2 :: 2, 1 :: 2>>, 2)
        [1, 2, 3, 0]

        iex> Itsy.Binary.r_unpack_reverse(<<4000 :: 12, 3 :: 12, 2 :: 12, 1 :: 12>>, 12)
        [1, 2, 3, 4000]

        iex> Itsy.Binary.r_unpack_reverse(<<"foo", 4 :: 3, 3 :: 3, 2 :: 3, 1 :: 3>>, 3, 4)
        [1, 2, 3, 4]

        iex> Itsy.Binary.r_unpack_reverse(<<1 :: 8, 0 :: 24>>, 32, nil, :little)
        [1]

        iex> Itsy.Binary.r_unpack_reverse(<<0 :: 24, 1 :: 8>>, 32)
        [1]

        iex> Itsy.Binary.r_unpack_reverse(<<255 :: 8>>, 8, nil, :big, :signed)
        [-1]
    """
    @spec r_unpack_reverse(bitstring, non_neg_integer, pos_integer | nil, endianness, sign) :: [integer]
    def r_unpack_reverse(packed, size, count \\ nil, endian \\ :big, sign \\ :unsigned)
    def r_unpack_reverse(packed, size, nil, endian, sign), do: l_unpack_reverse(packed, size, nil, endian, sign)
    def r_unpack_reverse(packed, size, count, endian, sign) do
        skip_count = bit_size(packed) - (count * size)
        <<_ :: size(skip_count), packed :: bitstring>> = packed
        l_unpack_reverse(packed, size, nil, endian, sign)
    end
end
