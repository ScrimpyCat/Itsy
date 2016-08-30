defmodule Itsy.Bit do
    use Bitwise

    @doc """
      A guard expression that checks whether the integer is a power of 2 or not.
    """
    @spec is_power_of_2(integer) :: Macro.t
    defmacro is_power_of_2(x), do: quote do: (unquote(x) &&& ~~~-unquote(x)) == 0

    @doc """
      Get the lowest unset bit.

        iex> Itsy.Bit.lowest_unset(0b1101)
        0b0010

        iex> Itsy.Bit.lowest_unset(0b1100)
        0b0001

        iex> Itsy.Bit.lowest_unset(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        0x0010000000000000000000000000000000000000000000000

        iex> Itsy.Bit.lowest_unset(0x1fefffffffffffffffffffffffffffffffffffffffffffffe)
        0x0000000000000000000000000000000000000000000000001

        iex> Itsy.Bit.lowest_unset(-7)
        2

        iex> Itsy.Bit.lowest_unset(-8)
        1
    """
    @spec lowest_unset(integer) :: integer
    def lowest_unset(x), do: ~~~x &&& (x + 1)

    @doc """
      Get the lowest set bit.

        iex> Itsy.Bit.lowest_set(0b1101)
        0b0001

        iex> Itsy.Bit.lowest_set(0b1100)
        0b0100

        iex> Itsy.Bit.lowest_set(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        0x0000000000000000000000000000000000000000000000001

        iex> Itsy.Bit.lowest_set(0x1fefffffffffffffffffffffffffffffffffffffffffffffe)
        0x0000000000000000000000000000000000000000000000002

        iex> Itsy.Bit.lowest_set(-7)
        1

        iex> Itsy.Bit.lowest_set(-8)
        8
    """
    @spec lowest_set(integer) :: integer
    def lowest_set(x), do: x &&& -x

    @doc """
      Get the highest set bit.

        iex> Itsy.Bit.highest_set(0b1101)
        0b1000

        iex> Itsy.Bit.highest_set(0b1100)
        0b1000

        iex> Itsy.Bit.highest_set(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        0x1000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.highest_set(0x1feff00000000000000000000000000000000000000000000)
        0x1000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.highest_set(-7)
        4

        iex> Itsy.Bit.highest_set(-8)
        8
    """
    @spec highest_set(integer) :: integer
    def highest_set(x) do
        x = mask(x)
        x ^^^ (x >>> 1)
    end

    @doc """
      Find the minimum power of 2 that is equal or greater than the value.

        iex> Itsy.Bit.min_power_of_2(0b1101)
        0b10000

        iex> Itsy.Bit.min_power_of_2(0b1000)
        0b1000

        iex> Itsy.Bit.min_power_of_2(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        0x2000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.min_power_of_2(0x1000000000000000000000000000000000000000000000000)
        0x1000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.min_power_of_2(-7)
        -8

        iex> Itsy.Bit.min_power_of_2(-8)
        -8
    """
    @spec min_power_of_2(integer) :: integer
    def min_power_of_2(0), do: 0
    def min_power_of_2(x) when x < 0, do: mask(x + 1)
    def min_power_of_2(x), do: mask(x - 1) + 1

    @doc """
      Create a mask for the entire value.

      An interesting property of this function is that a positive integer produces a mask that
      is the opposite to a negative integer.

        iex> Itsy.Bit.mask(0b1101)
        0b1111

        iex> Itsy.Bit.mask(0b1100)
        0b1111

        iex> Itsy.Bit.mask(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        0x1ffffffffffffffffffffffffffffffffffffffffffffffff

        iex> Itsy.Bit.mask(0x1feff00000000000000000000000000000000000000000000)
        0x1ffffffffffffffffffffffffffffffffffffffffffffffff

        iex> Itsy.Bit.mask(-3)
        -4

        iex> Itsy.Bit.mask(-4)
        -8

        iex> :erlang.band(0b1111, Itsy.Bit.mask(2)) + :erlang.band(0b1111, Itsy.Bit.mask(-2))
        0b1111

        iex> { :erlang.band(0b1111, Itsy.Bit.mask(2)), :erlang.band(0b1111, Itsy.Bit.mask(-2)) }
        { 3, 12 }
    """
    @spec mask(integer) :: integer
    def mask(x) when x < 0, do: unmask(mask(-x))
    def mask(x), do: mask(x, 1)

    defp mask(x, _) when is_power_of_2(x + 1), do: x
    defp mask(x, size), do: mask(x ||| (x >>> size), size <<< 1)

    @doc """
      Create a mask for the range that exceeds the value. Produces a mask opposite to `mask/1`.

      An interesting property of this function is that a positive integer produces a mask that
      is the opposite to a negative integer.

        iex> Itsy.Bit.unmask(0b1101)
        -0b10000

        iex> Itsy.Bit.unmask(0b1100)
        -0b10000

        iex> Itsy.Bit.unmask(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        -0x2000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.unmask(0x1feff00000000000000000000000000000000000000000000)
        -0x2000000000000000000000000000000000000000000000000

        iex> Itsy.Bit.unmask(-3)
        3

        iex> Itsy.Bit.unmask(-4)
        7

        iex> :erlang.band(0b1111, Itsy.Bit.unmask(2)) + :erlang.band(0b1111, Itsy.Bit.unmask(-2))
        0b1111

        iex> { :erlang.band(0b1111, Itsy.Bit.unmask(2)), :erlang.band(0b1111, Itsy.Bit.unmask(-2)) }
        { 12, 3 }
    """
    @spec unmask(integer) :: integer
    def unmask(x) when x < 0, do: ~~~unmask(-x)
    def unmask(x), do: ~~~mask(x)

    @doc """
      Masks the lower bits of a power of 2 value. Value must be a power of 2.

        iex> Itsy.Bit.mask_lower_power_of_2(0b1101)
        ** (Itsy.PowerOf2Error) value 13 is not a power of 2

        iex> Itsy.Bit.mask_lower_power_of_2(0b1000)
        0b0111

        iex> Itsy.Bit.mask_lower_power_of_2(0x10e0000000000000000000000000000000000000000000000)
        ** (Itsy.PowerOf2Error) value 6620380736540639868108059157289335673232953007833161400320 is not a power of 2

        iex> Itsy.Bit.mask_lower_power_of_2(0x1000000000000000000000000000000000000000000000000)
        0x0ffffffffffffffffffffffffffffffffffffffffffffffff
    """
    @spec mask_lower_power_of_2(integer) :: integer
    def mask_lower_power_of_2(x) when (x != 0) and is_power_of_2(x), do: ~~~-x
    def mask_lower_power_of_2(x) when x < 0, do: -mask_lower_power_of_2(-x)
    def mask_lower_power_of_2(x), do: raise Itsy.PowerOf2Error, value: x

    @doc """
      Count the set bits.

        iex> Itsy.Bit.count(0b1101)
        3

        iex> Itsy.Bit.count(0b1100)
        2

        iex> Itsy.Bit.count(0x1feffffffffffffffffffffffffffffffffffffffffffffff)
        192

        iex> Itsy.Bit.count(0x1feff00000000000000000000000000000000000000000000)
        16
    """
    @spec count(non_neg_integer) :: non_neg_integer
    def count(x) when x >= 0, do: count(x, 0)

    defp count(0, total), do: total
    defp count(x, total) do
        c = x &&& 0xffffffff
        c = c - ((c >>> 1) &&& 0x55555555)
        c = (c &&& 0x33333333) + ((c >>> 2) &&& 0x33333333)
        c = (c + (c >>> 4)) &&& 0x0f0f0f0f
        c = ((c * 0x01010101) &&& 0xffffffff) >>> 24

        count(x >>> 32, total + c)
    end

    @doc """
      Sets `n` least significant bits.

        iex> Itsy.Bit.set(0)
        0

        iex> Itsy.Bit.set(4)
        0b1111

        iex> Itsy.Bit.set(70)
        0b1111111111111111111111111111111111111111111111111111111111111111111111

        iex> Itsy.Bit.set(192)
        0xffffffffffffffffffffffffffffffffffffffffffffffff
    """
    @spec set(non_neg_integer) :: non_neg_integer
    def set(n) when n >= 0, do: (1 <<< n) - 1

    @doc """
      Check whether the value is a power of 2.

        iex> Itsy.Bit.power_of_2?(0b1101)
        false

        iex> Itsy.Bit.power_of_2?(0b1000)
        true

        iex> Itsy.Bit.power_of_2?(0x10e0000000000000000000000000000000000000000000000)
        false

        iex> Itsy.Bit.power_of_2?(0x1000000000000000000000000000000000000000000000000)
        true
    """
    @spec power_of_2?(integer) :: boolean()
    def power_of_2?(x) when x < 0, do: power_of_2?(-x)
    def power_of_2?(x), do: (x != 0) && is_power_of_2(x)
end
