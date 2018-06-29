defmodule Itsy.BinaryTest do
    use ExUnit.Case
    doctest Itsy.Binary
    require Itsy.Binary

    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    |> String.graphemes
    |> Enum.with_index
    |> Itsy.Binary.encoder(encode: :encode64, decode: :decode64)

    setup do
        sequences = [
            "",
            "a",
            "ab",
            "abc",
            "abcd",
            "abcde",
            "abcdef",
            "abcdefg",
            "abcdefgh",
            "abcdefghi",
            <<0 :: 8>>,
            <<0 :: 8, 1 :: 8>>,
            <<0 :: 8, 1 :: 8, 2 :: 8>>,
            <<0 :: 8, 1 :: 8, 2 :: 8, 3 :: 8>>,
        ]

        { :ok, %{ sequences: sequences } }
    end

    describe "base64" do
        test "encoding" do
            data = <<0 :: 6>>
            assert "A" == encode64(data)
            assert <<"A", 0 :: 8>> == encode64(data, multiple: 2)
            assert <<"A", 0 :: 16>> == encode64(data, multiple: 3)
            assert <<"A", 0 :: 24>> == encode64(data, multiple: 4)
            assert "A12312" == encode64(data, multiple: 6, pad_chr: "123")
            assert "AЯЯЯЯЯ" == encode64(data, multiple: 6, pad_chr: "Я")
            assert "A" == encode64(data, pad_bit: -1)
            assert "AA" == encode64(data, [], "A")

            data = <<0 :: 1>>
            assert "A" == encode64(data)
            assert encode64(<<data :: bitstring, -1 :: 5>>) == encode64(data, pad_bit: -1)
            assert encode64(<<data :: bitstring, 2 :: 5>>) == encode64(data, pad_bit: 2)
            assert encode64(<<data :: bitstring, 0 :: 5>>) == encode64(data, pad_bit: 32)
        end

        test "decoding" do
            assert :error == decode64("~")
            assert :error == decode64("A~")
            assert :error == decode64("AA~")
            assert :error == decode64("AA==~", pad_chr: "=")

            assert { :ok, <<0 :: 6>> } == decode64("A===", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 12>> } == decode64("AA==", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 18>> } == decode64("AAA=", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 24>> } == decode64("AAAA", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 6, 0 :: 6>> } == decode64("A===A===", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 12, 0 :: 6>> } == decode64("AA==A===", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 18, 0 :: 6>> } == decode64("AAA=A===", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 24, 0 :: 6>> } == decode64("AAAAA===", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 6, 0 :: 12>> } == decode64("A===AA==", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 12, 0 :: 12>> } == decode64("AA==AA==", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 18, 0 :: 12>> } == decode64("AAA=AA==", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 24, 0 :: 12>> } == decode64("AAAAAA==", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 6, 0 :: 18>> } == decode64("A===AAA=", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 12, 0 :: 18>> } == decode64("AA==AAA=", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 18, 0 :: 18>> } == decode64("AAA=AAA=", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 24, 0 :: 18>> } == decode64("AAAAAAA=", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 6, 0 :: 24>> } == decode64("A===AAAA", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 12, 0 :: 24>> } == decode64("AA==AAAA", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 18, 0 :: 24>> } == decode64("AAA=AAAA", pad_chr: "=", bits: true)
            assert { :ok, <<0 :: 24, 0 :: 24>> } == decode64("AAAAAAAA", pad_chr: "=", bits: true)

            assert { :ok, <<0 :: 0>> } == decode64("A===", pad_chr: "=")
            assert { :ok, <<0 :: 8>> } == decode64("AA==", pad_chr: "=")
            assert { :ok, <<0 :: 16>> } == decode64("AAA=", pad_chr: "=")
            assert { :ok, <<0 :: 24>> } == decode64("AAAA", pad_chr: "=")
            assert { :ok, <<0 :: 0, 0 :: 0>> } == decode64("A===A===", pad_chr: "=")
            assert { :ok, <<0 :: 8, 0 :: 0>> } == decode64("AA==A===", pad_chr: "=")
            assert { :ok, <<0 :: 16, 0 :: 0>> } == decode64("AAA=A===", pad_chr: "=")
            assert { :ok, <<0 :: 24, 0 :: 0>> } == decode64("AAAAA===", pad_chr: "=")
            assert { :ok, <<0 :: 0, 0 :: 8>> } == decode64("A===AA==", pad_chr: "=")
            assert { :ok, <<0 :: 8, 0 :: 8>> } == decode64("AA==AA==", pad_chr: "=")
            assert { :ok, <<0 :: 16, 0 :: 8>> } == decode64("AAA=AA==", pad_chr: "=")
            assert { :ok, <<0 :: 24, 0 :: 8>> } == decode64("AAAAAA==", pad_chr: "=")
            assert { :ok, <<0 :: 0, 0 :: 16>> } == decode64("A===AAA=", pad_chr: "=")
            assert { :ok, <<0 :: 8, 0 :: 16>> } == decode64("AA==AAA=", pad_chr: "=")
            assert { :ok, <<0 :: 16, 0 :: 16>> } == decode64("AAA=AAA=", pad_chr: "=")
            assert { :ok, <<0 :: 24, 0 :: 16>> } == decode64("AAAAAAA=", pad_chr: "=")
            assert { :ok, <<0 :: 0, 0 :: 24>> } == decode64("A===AAAA", pad_chr: "=")
            assert { :ok, <<0 :: 8, 0 :: 24>> } == decode64("AA==AAAA", pad_chr: "=")
            assert { :ok, <<0 :: 16, 0 :: 24>> } == decode64("AAA=AAAA", pad_chr: "=")
            assert { :ok, <<0 :: 24, 0 :: 24>> } == decode64("AAAAAAAA", pad_chr: "=")
        end

        test "encoding/decoding", %{ sequences: sequences } do
            Enum.each(sequences, fn data ->
                padding = Itsy.Binary.encoder_padding(64)
                assert { :ok, data } == decode64(encode64(data))
                assert { :ok, data } == decode64(encode64(data, multiple: padding, pad_chr: "="), pad_chr: "=")
                assert { :ok, <<"test", data :: binary>> } == decode64(encode64(data, multiple: padding, pad_chr: "="), [pad_chr: "="], "test")
                assert { :ok, <<"test", data :: binary>> } == decode64(encode64("test", multiple: padding, pad_chr: "=") <> encode64(data, multiple: padding, pad_chr: "="), pad_chr: "=")
                assert { :ok, <<"test", data :: binary>> } == decode64(encode64(data, [multiple: padding, pad_chr: "="], encode64("test", multiple: padding, pad_chr: "=")), pad_chr: "=")
            end)
        end

        test "correctness", %{ sequences: sequences } do
            Enum.each(sequences, fn data ->
                assert Base.encode64(data, padding: false) == encode64(data)
                assert Base.encode64(data, padding: true) == encode64(data, multiple: Itsy.Binary.encoder_padding(64), pad_chr: "=")
            end)
        end
    end
end
