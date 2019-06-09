defmodule Itsy.Binary do
    @type endianness :: :big | :little | :native
    @type signedness :: :unsigned | :signed
    @type position :: :high | :low
    @type encodable :: number | bitstring
    @type decode_type :: :integer | :float | :bitstring
    @type decoder :: decode_type | ((bitstring, [encodable]) -> decode_type)
    @type packing_options :: [position: position, into: bitstring, endian: endianness, reverse: boolean]
    @type unpacking_options :: [position: position, count: nil | non_neg_integer, endian: endianness, sign: signedness, reverse: boolean, decoder: decoder]
    @type encoder_options :: [encode: atom, decode: atom, private: boolean, docs: boolean]
    @type encode_options :: [multiple: pos_integer, pad_chr: String.t, pad_bit: integer]
    @type decode_options :: [bits: boolean, pad_chr: String.t]

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

    @doc """
      Get the number of bits one character of an encoding will represent.

        iex> Itsy.Binary.encoder_size(1)
        0

        iex> Itsy.Binary.encoder_size(2)
        1

        iex> Itsy.Binary.encoder_size(4)
        2

        iex> Itsy.Binary.encoder_size(8)
        3

        iex> Itsy.Binary.encoder_size(16)
        4

        iex> Itsy.Binary.encoder_size(:erlang.bsl(1, 1000))
        1000
    """
    @spec encoder_size(pos_integer) :: integer
    def encoder_size(count), do: Itsy.Bit.count(Itsy.Bit.mask_lower_power_of_2(count))

    defp padding(count, n) when rem(count * n, 8) == 0, do: n
    defp padding(count, n), do: padding(count, n + 1)

    @doc """
      Get the number of characters needed for an encoding, in order to pad to a
      whole byte.

      This value can be given to the `:multiples` option when encoding, in order
      to obtain an encoding with the minimum amount of padding needed so it can
      be concatenated with other encoded data.

        iex> Itsy.Binary.encoder_padding(1)
        1

        iex> Itsy.Binary.encoder_padding(2)
        8

        iex> Itsy.Binary.encoder_padding(4)
        4

        iex> Itsy.Binary.encoder_padding(8)
        8

        iex> Itsy.Binary.encoder_padding(16)
        2

        iex> Itsy.Binary.encoder_padding(:erlang.bsl(1, 1000))
        1
    """
    @spec encoder_padding(pos_integer) :: pos_integer
    def encoder_padding(count), do: padding(encoder_size(count), 1)

    @doc """
      Generate an encoding for the given power of 2 length character set.

      The encoder will generate two functions `encode/3` and `decode/3`. These
      functions will be used to encode data using your specified encoding scheme,
      and decode strings with that encoding. The naming of these functions can
      be changed by setting the `:encode` and `:decode` options.

      By default the functions are made public (including documentation). They
      can be made private by setting the `:private` option to `true`. Optionally
      only the docs can be removed by setting `:docs` to `false`.

      The generated documentation includes some doctests. To use these tests make
      sure the module is specified as a doctest in your test code. If you do not
      wish for these to be included in your tests (but still want the docs), then
      simply exclude (`:except` option) the two functions from your
      `ExUnit.DocTest.doctests/2`.

        defmodule MyBase4 do
            require Itsy.Binary

            "ABCD"
            |> String.graphemes
            |> Enum.with_index
            |> Itsy.Binary.encoder
        end

        MyBase4.encode("hello") |> MyBase4.decode
    """
    @spec encoder([{ char :: String.t, value :: non_neg_integer },...], encoder_options) :: Macro.t
    defmacro encoder(charset, opts \\ []) do
        quote bind_quoted: [
            charset: charset,
            encode: opts[:encode] || :encode,
            decode: opts[:decode] || :decode
        ] do
            charset_count = length(charset)
            encoding_size = Itsy.Binary.encoder_size(charset_count)
            get_size = if Enum.any?(charset, fn { c, _ } -> byte_size(c) > 1 end) do
                { fun, _, _ } = quote(do: String.length)
                fun
            else
                :byte_size
            end

            # for docs examples
            { encoding_chr0, _ } = Enum.find(charset, "", fn
                { _, 0 } -> true
                _ -> false
            end)
            chars = Enum.map(charset, fn { c, _ } -> c end)
            paddable_chr =
                Stream.iterate(0, &(&1 + 1))
                |> Stream.map(fn i ->
                    size = Itsy.Bit.mask(i)
                    size = size + 8 - (size - div(size, 8))
                    <<i :: size(size)-big>>
                end)
                |> Stream.filter(&(String.printable?(&1) && &1))
                |> Stream.filter(&(!Enum.any?(chars, fn c -> &1 == c end)))
                |> Enum.at(0, "")

            @doc """
              Decodes encoded strings produced by the `#{encode}/3` function.

              By default the decoded data will be stripped of any bits that do not
              fit in the byte boundaries. This behaviour can be changed by setting
              the `:bits` option to `true`. The resulting `bitstring` may contain
              some pad bits.

              If the encoded string contains some padding characters these will
              to be specified so they can be removed. Set the `:pad_chr` option
              to the padding sequence that was used when encoding this data.
              Multiple encoded strings with the same padding sequence can be
              chained together, and will be decoded into a contiguous set of
              data.

                iex> #{inspect __MODULE__}.#{decode}(#{inspect encoding_chr0})
                { :ok, #{inspect Itsy.Binary.pack([0], 8 * div(encoding_size, 8))} }

                iex> #{inspect __MODULE__}.#{decode}(#{inspect encoding_chr0}, bits: true)
                { :ok, #{inspect <<0 :: size(encoding_size)>>} }

                iex> #{inspect __MODULE__}.#{decode}(#{inspect String.pad_trailing(encoding_chr0, Itsy.Binary.encoder_padding(charset_count), paddable_chr)}, bits: true, pad_chr: #{inspect paddable_chr})
                { :ok, #{inspect <<0 :: size(encoding_size)>>} }

                iex> #{inspect __MODULE__}.#{decode}(#{inspect encoding_chr0 <> encoding_chr0 <> encoding_chr0 <> encoding_chr0}, bits: true)
                { :ok, #{inspect <<0 :: size(encoding_size), 0 :: size(encoding_size), 0 :: size(encoding_size), 0 :: size(encoding_size)>>} }
            """
            @spec unquote(decode)(bitstring, Itsy.Binary.decode_options, bitstring) :: { :ok, bitstring } | :error
            def unquote(decode)(encoding, opts \\ [], data \\ <<>>)
            for { chr, index } <- charset do
                def unquote(decode)(<<unquote(chr) :: binary, encoding :: binary>>, opts, data), do: unquote(decode)(encoding, opts, <<data :: bitstring, unquote(index) :: size(unquote(encoding_size))>>)
            end
            def unquote(decode)("", opts, data) do
                if opts[:bits] do
                    { :ok, data }
                else
                    bytes = div(bit_size(data), 8)
                    <<data :: binary-size(bytes), _ :: bitstring>> = data
                    { :ok, data }
                end
            end
            def unquote(decode)(encoding, [encoding|_], _), do: :error
            def unquote(decode)(encoding, opts, data) do
                { :ok, data } = unquote(decode)("", opts, data)

                case opts[:pad_chr] || "" do
                    "" -> encoding
                    pad ->
                        String.graphemes(pad)
                        |> Stream.cycle
                        |> Enum.reduce_while(encoding, fn c, acc ->
                            size = byte_size(c)
                            case acc do
                                <<^c :: binary-size(size), acc :: binary>> -> { :cont, acc }
                                _ -> { :halt, acc }
                            end
                        end)
                end
                |> unquote(decode)([encoding|opts], data)
            end

            @doc """
              Encodes data into a string which can be decoded using the `#{decode}/3`
              function.

              The encoded data will pad any bits needed to make the entire data
              fit within byte boundaries. By default this padding will be using
              `0` bits, however this can be changed by setting the option `:pad_bit`
              to the specific bit sequence desired (note that only the part of the
              bit sequence needed to reach a byte boundary will be used).

              Optionally an encoded string can have additional padding put onto
              the final encoding (e.g. if you wanted to pack encodings together).
              To enable this behaviour set `:multiple` to the multiples of
              characters needed in the encoded string. e.g. A value of `1` (the
              default) will not require any padding, a value of `2` will require
              padding if there is only an odd number of characters.

              By default the encoded string padding will use `0` bytes, if a
              specific padding character(s) is desired, this can be done by
              setting the `:pad_chr` option to the string to use as the padding
              sequence (note that only the part of the character sequence needed
              to reach the specified multiple will be used).

                iex> #{inspect __MODULE__}.#{encode}(#{inspect <<0 :: size(encoding_size)>>})
                #{inspect if(encoding_size > 0, do: encoding_chr0, else: "")}

                iex> #{inspect __MODULE__}.#{encode}(#{inspect <<0 :: size(encoding_size)>>}, multiple: #{inspect Itsy.Binary.encoder_padding(charset_count)}, pad_chr: #{inspect paddable_chr})
                #{inspect if(encoding_size > 0, do: String.pad_trailing(encoding_chr0, Itsy.Binary.encoder_padding(charset_count), paddable_chr), else: "")}

                iex> #{inspect __MODULE__}.#{encode}(#{inspect <<0 :: size(encoding_size), 0 :: size(encoding_size), 0 :: size(encoding_size), 0 :: size(encoding_size)>>})
                #{inspect if(encoding_size > 0, do: String.pad_leading("", 4, encoding_chr0), else: "")}
            """
            @spec unquote(encode)(bitstring, Itsy.Binary.encode_options, binary) :: binary
            def unquote(encode)(data, opts \\ [], encoding \\ "")
            if encoding_size > 0 do
                for { chr, index } <- charset do
                    def unquote(encode)(<<unquote(index) :: size(unquote(encoding_size)), data :: bitstring>>, opts, encoding), do: unquote(encode)(data, opts, encoding <> unquote(<<chr :: binary>>))
                end
            else
                def unquote(encode)(data, opts, encoding) when bit_size(data) > 0, do: unquote(encode)(<<>>, opts, encoding)
            end
            def unquote(encode)(<<>>, opts, encoding) do
                multiple = opts[:multiple] || 1
                case rem(unquote(get_size)(encoding), multiple) do
                    0 -> encoding
                    r ->
                        case opts[:pad_chr] do
                            nil ->
                                pad_size = (multiple - r) * 8
                                <<encoding :: binary, 0 :: size(pad_size)>>
                            pad_chr -> encoding <> String.pad_trailing("", multiple - r, pad_chr)
                        end
                end
            end
            def unquote(encode)(data, opts, encoding) do
                padding_size = unquote(encoding_size) - bit_size(data)
                unquote(encode)(<<data :: bitstring, (opts[:pad_bit] || 0) :: size(padding_size)>>, opts, encoding)
            end
        end
        |> Macro.postwalk(fn
            { :def, context, body } -> { if(opts[:private], do: :defp, else: :def), context, body }
            node = { :@, context, [{ :doc, doc_context, _ }|_] } -> if(opts[:private] || !(opts[:docs] || true), do: { :@, context, [{ :doc, doc_context, [false] }] }, else: node)
            node -> node
        end)
    end
end
