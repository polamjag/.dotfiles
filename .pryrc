require 'open-uri'

Pry.config.color = true

Pry.commands.alias_command 'c', 'continue'
Pry.commands.alias_command 's', 'step'
Pry.commands.alias_command 'n', 'next'

Pry.config.prompt = proc do |obj, nest_level, _pry_|
  version = ''
  version << "\001\e[0;31m\002"
  version << "#{RUBY_VERSION}"
  version << "\001\e[0m\002"

  "#{version} #{Pry.config.prompt_name}(#{Pry.view_clip(obj)})> "
end
