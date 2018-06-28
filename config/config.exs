# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

if Mix.env == :dev do
    import_config "simple_markdown_rules.exs"

    config :ex_doc, :markdown_processor, ExDocSimpleMarkdown
end
