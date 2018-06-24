defmodule Itsy.Binary do
    @type endianness :: :big | :little | :native

    @doc """
      Pack a list of integers in reverse order into the high-order bits of a
      bitstring.

      The most significant bit will be based on the last number in the list.

      The bytes order defaults to big endian, this can be overriden by passing
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

      The bytes order defaults to big endian, this can be overriden by passing
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

      The bytes order defaults to big endian, this can be overriden by passing
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

      The bytes order defaults to big endian, this can be overriden by passing
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
end
