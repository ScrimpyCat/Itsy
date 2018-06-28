# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

if Mix.env == :dev do
    import_config "simple_markdown_rules.exs"

    # config :simple_markdown_extension_highlight_js,
    #     source: Enum.at(Path.wildcard(Path.join(Mix.Project.deps_path(), "ex_doc/formatters/html/dist/*.js")), 0, "")

    # config :ex_doc_simple_markdown, [
    #     rules: fn rules ->
    #         :ok = SimpleMarkdownExtensionHighlightJS.setup
    #         SimpleMarkdownExtensionBlueprint.add_rule(rules)
    #     end
    # ]

    config :ex_doc, :markdown_processor, ExDocSimpleMarkdown
end
