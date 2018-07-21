defmodule Itsy.Float do
    use Bitwise
    require Itsy.Bit

    alias Itsy.Bit

    @type infinity :: :"-inf" | :"+inf"
    @type rounding :: :down | :up | :even
    @type sign_size :: non_neg_integer
    @type exponent_size :: non_neg_integer
    @type mantissa_size :: non_neg_integer
    @type encoding :: { sign_size, exponent_size, mantissa_size }
    @type precision :: encoding | 16 | 32 | 64 | 128 | 256
    @type options :: [rounding: rounding, precision: precision, raw: boolean]

    @spec sign(float | infinity, []) :: 0 | 1
    @spec sign(bitstring, [precision: precision]) :: 0 | 1
    def sign(v, opts \\ [])
    def sign(v, _) when is_number(v), do: sign(<<v :: float>>)
    def sign(:"+inf", _), do: 0
    def sign(:"-inf", _), do: 1
    def sign(v, opts) do
        { sp, _, _ } = format_options(opts)[:precision]
        <<s :: size(sp), _ :: bitstring>> = v
        s
    end

    @spec exponent(float | infinity, []) :: integer
    @spec exponent(bitstring, [precision: precision]) :: integer
    def exponent(v, opts \\ [])
    def exponent(v, _) when is_number(v), do: exponent(<<v :: float>>)
    def exponent(v, opts) when v in [:"+inf", :"-inf"] do
        { _, ep, _ } = format_options(opts)[:precision]
        -Bit.set(ep - 1)
    end
    def exponent(v, opts) do
        { sp, ep, _ } = format_options(opts)[:precision]
        <<_s :: size(sp), e :: size(ep), _ :: bitstring>> = v
        e - Bit.set(ep - 1)
    end

    @spec mantissa(float | infinity, []) :: non_neg_integer
    @spec mantissa(bitstring, [precision: precision]) :: non_neg_integer
    def mantissa(v, opts \\ [])
    def mantissa(v, _) when is_number(v), do: mantissa(<<v :: float>>)
    def mantissa(v, _) when v in [:"+inf", :"-inf"], do: 0
    def mantissa(v, opts) do
        { sp, ep, mp } = format_options(opts)[:precision]
        <<_s :: size(sp), _e :: size(ep), m :: size(mp)>> = v
        m
    end

    @spec format_options(options) :: [raw: boolean, rounding: rounding, precision: encoding]
    defp format_options(opts) do
        opts = Keyword.merge([raw: false, precision: 64, rounding: :even], opts)
        case opts[:precision] do
            256 -> Keyword.replace!(opts, :precision, { 1, 19, 236 })
            128 -> Keyword.replace!(opts, :precision, { 1, 15, 112 })
            64 -> Keyword.replace!(opts, :precision, { 1, 11, 52 })
            32 -> Keyword.replace!(opts, :precision, { 1, 8, 23 })
            16 -> Keyword.replace!(opts, :precision, { 1, 5, 10 })
            precision when is_tuple(precision) -> opts
        end
    end

    @doc """
      Create a float from an integer value and exponent.

      The default precision is set to a binary64, but this can be changed by
      setting the `:precision` option. This can either be passed in a standard
      IEEE 754 encoding format, or the precision can be set for each part of
      the float (sign, encoding, mantissa/significand).

      By default the return value will be a float converting from the underlying
      precision, or one of the infinity atoms. However if `:raw` is set to `true`
      the return type will be the unconverted binary for the given precision.

      The rounding defaults to the standard IEEE 754 rounding mode of round half
      to even (`:even`). If this is not desired, an alternative rounding mode
      can be specified using `:rounding`.

        iex> Itsy.Float.new(0)
        ...> 0.0

        iex> Itsy.Float.new(1, 20)
        ...> 1.0e20

        iex> Itsy.Float.new(1, -20)
        ...> 1.0e-20

        iex> Itsy.Float.new(1, -1, rounding: :even)
        0.1

        iex> Itsy.Float.new(1, -1, rounding: :down)
        0.09999999999999999

        iex> Itsy.Float.new(3, -1, rounding: :even)
        0.3

        iex> Itsy.Float.new(3, -1, rounding: :up)
        0.30000000000000004

        iex> Itsy.Float.new(2225073858507201, -323)
        2.225073858507201e-308

        iex> Itsy.Float.new(17976931348623157, 292)
        1.7976931348623157e308

        iex> Itsy.Float.new(13, -1, precision: { 1, 2, 2 })
        1.25

        iex> Itsy.Float.new(14, -1, precision: { 1, 2, 2 })
        1.5

        iex> Itsy.Float.new(14, -1, precision: { 1, 2, 2 }, raw: true)
        <<6::size(5)>>
    """
    @spec new(integer, integer, options) :: float | infinity | bitstring
    def new(value, exponent \\ 0, opts \\ []) do
        opts = format_options(opts)
        encoding = { sp, ep, mp } = opts[:precision]
        e_max = Bit.set(ep - 1)
        { e, m } = if(value != 0, do: integer_to_float(value, exponent * -1, opts[:rounding], encoding), else: { -e_max, 0 })

        s = case sp do
            0 -> 0
            _ -> boolean_to_integer(value < 0)
        end

        if e > e_max do
            if opts[:raw] do
                <<s :: size(sp), -1 :: size(ep), 0 :: size(mp)>>
            else
                if(s == 0, do: :"+inf", else: :"-inf")
            end
        else
            if opts[:raw] do
                <<s :: size(sp), e + e_max :: size(ep), m :: size(mp)>>
            else
                e = e + 1023

                m = case mp do
                    0 -> 0
                    size when size <= 52 -> min(m, Bit.set(mp)) <<< (52 - size)
                    size -> min(m, Bit.set(mp)) >>> abs(52 - size)
                end

                <<f :: float>> = <<s :: 1, e :: 11, m :: 52>>
                f
            end
        end
    end

    @spec whole_to_float(integer, mantissa_size) :: { integer, integer }
    defp whole_to_float(v, mp) do
        e = Bit.highest_set(v) |> Bit.mask_lower_power_of_2
        m = e &&& v
        e = Bit.count(e)

        { e, m <<< (mp - e) }
    end

    @spec rounding(integer, integer, integer, rounding) :: 0 | 1
    defp rounding(_, _, _, :down), do: 0
    defp rounding(_, _, _, :up), do: 1
    defp rounding(v, precision, m, :even) do
        case (rem(v, precision) * 2) do
            v when v == precision -> m &&& 1
            v when v > precision -> 1
            _ -> 0
        end
    end

    @spec fraction_to_mantissa(integer, integer, integer, rounding, mantissa_size, integer, integer) :: integer
    defp fraction_to_mantissa(v, e, precision, rounding, size, m \\ 0, index \\ 0)
    defp fraction_to_mantissa(0, _, _, _, _, m, _), do: m
    defp fraction_to_mantissa(v, _, precision, rounding, size, m, size), do: m + rounding(v, precision, m, rounding)
    defp fraction_to_mantissa(v, e, precision, rounding, size, m, index) do
        v = rem(v, precision) * 2
        m = m ||| ((boolean_to_integer(v >= precision) <<< ((size - 1) - index)) >>> e)
        fraction_to_mantissa(v, e, precision, rounding, size, m, index + 1)
    end

    @spec fraction_to_exponent(integer, integer, integer, integer, integer) :: { integer, integer }
    defp fraction_to_exponent(v, precision, max, e \\ nil, index \\ 0)
    defp fraction_to_exponent(v, _, _, e, _) when e != nil, do: { e, v }
    defp fraction_to_exponent(0, _, _, _, _), do: { nil, 0 }
    defp fraction_to_exponent(v, _, max, _, max), do: { max, v }
    defp fraction_to_exponent(v, precision, opts, _, index) do
        v = rem(v, precision) * 2
        fraction_to_exponent(v, precision, opts, if(v >= precision, do: index), index + 1)
    end

    @spec integer_to_float(integer, integer, rounding, encoding) :: { integer, integer }
    defp integer_to_float(v, precision, rounding, encoding) when precision < 0 do
        precision = pow10(abs(precision))
        integer_to_float(0, abs(v) * precision, precision, rounding, encoding)
    end
    defp integer_to_float(v, precision, rounding, encoding) do
        precision = pow10(precision)
        integer_to_float(abs(v), div(abs(v), precision), precision, rounding, encoding)
    end

    @spec integer_to_float(integer, integer, integer, rounding, encoding) :: { integer, integer }
    defp integer_to_float(v, 0, precision, rounding, { _, ep, mp }) do
        { e, v } = fraction_to_exponent(v, precision, Bit.set(ep - 1) - 1)
        m = fraction_to_mantissa(v, 0, precision, rounding, mp)
        { ~~~e, m }
    end
    defp integer_to_float(v, i, precision, rounding, { _, _, mp }) do
        { e, m } = whole_to_float(i, mp)
        m = fraction_to_mantissa(v, e, precision, rounding, mp, m)
        { e, m }
    end

    @spec pow10(non_neg_integer) :: non_neg_integer
    defp pow10(n), do: pow10(1, n)

    defp pow10(x, 0), do: x
    defp pow10(x, n), do: pow10(x * 10, n - 1)

    defp boolean_to_integer(false), do: 0
    defp boolean_to_integer(true), do: 1
end
