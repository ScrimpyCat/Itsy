defmodule Itsy.Float do
    use Bitwise
    alias Itsy.Bit

    @spec sign(float) :: integer
    def sign(v) do
        <<s :: size(1), _ :: bitstring>> = <<v :: float>>
        s
    end

    @spec exponent(float) :: integer
    def exponent(v) do
        <<_s :: size(1), e :: size(11), _ :: bitstring>> = <<v :: float>>
        e - 1023
    end

    @spec mantissa(float) :: integer
    def mantissa(v) do
        <<_s :: size(1), _e :: size(11), m :: size(52)>> = <<v :: float>>
        m
    end

    @spec new(integer, integer) :: float
    def new(v, precision \\ 0), do: integer_to_float(v, precision)

    @spec whole_to_float(integer) :: float
    defp whole_to_float(v) do
        e = Bit.highest_set(v) |> Bit.mask_lower_power_of_2
        m = e &&& v
        e = Bit.count(e)

        { e, m <<< (52 - e) }
    end

    defp fraction_to_mantissa(v, e, precision, m \\ 0, index \\ 0)
    defp fraction_to_mantissa(0, _, _, m, _), do: m
    defp fraction_to_mantissa(_, _, _, m, 51), do: m
    defp fraction_to_mantissa(v, e, precision, m, index) do
        v = rem(v, precision) * 2
        m = m ||| ((boolean_to_integer(v >= precision) <<< (51 - index)) >>> e)
        fraction_to_mantissa(v, e, precision, m, index + 1)
    end

    defp fraction_to_exponent(v, precision, e \\ nil, index \\ 0)
    defp fraction_to_exponent(v, _, e, _) when e != nil, do: { e, v }
    defp fraction_to_exponent(0, _, e, _), do: { nil, 0 }
    defp fraction_to_exponent(v, _, e, 1022), do: { 1022, v }
    defp fraction_to_exponent(v, precision, e, index) do
        v = rem(v, precision) * 2
        fraction_to_exponent(v, precision, if(v >= precision, do: index), index + 1)
    end

    defp integer_to_float(v, precision) do
        precision = pow10(precision)
        { e, m } = integer_to_float(abs(v), div(abs(v), precision), precision)
        <<f :: float>> = <<boolean_to_integer(v < 0) :: size(1), e + 1023 :: size(11), m :: size(52)>>
        f
    end

    defp integer_to_float(v, 0, precision) do
        { e, v } = fraction_to_exponent(v, precision)
        m = fraction_to_mantissa(v, 0, precision)
        { ~~~e, m }
    end
    defp integer_to_float(v, i, precision) do
        { e, m } = whole_to_float(i)
        m = fraction_to_mantissa(v, e, precision, m)
        { e, m }
    end

    @spec pow10(non_neg_integer) :: non_neg_integer
    defp pow10(n), do: pow10(1, n)

    defp pow10(x, 0), do: x
    defp pow10(x, n), do: pow10(x * 10, n - 1)

    defp boolean_to_integer(false), do: 0
    defp boolean_to_integer(true), do: 1
end
