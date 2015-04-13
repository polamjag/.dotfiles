require 'open-uri'

Pry.config.color = true

Pry.commands.alias_command 'c', 'continue'
Pry.commands.alias_command 's', 'step'
Pry.commands.alias_command 'n', 'next'

Pry.config.prompt = proc do |obj, nest_level, _pry_|
  "#{RUBY_VERSION} #{Pry.config.prompt_name}(#{Pry.view_clip(obj)})> "
end
