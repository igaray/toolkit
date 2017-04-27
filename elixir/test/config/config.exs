# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# By default, the umbrella project as well as each child
# application will require this configuration file, ensuring
# they all use the same configuration. While one could
# configure all applications here, we prefer to delegate
# back to each application for organization purposes.
import_config "../apps/*/config/config.exs"

# Sample configuration (overrides the imported configuration above):
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]

config :logger,
  backends: [:console],
  compile_time_purge_level: :debug,
  level: :debug,
  utc_log: false,
  truncate: :infinity, # default: 8192
  sync_threshold: 20,
  handle_otp_reports: true,
  handle_sasl_reports: false, # default: false
  discard_threshold_for_error_logger: 500,
  translator_inspect_opts: []

config :logger, :console,
  level: :debug,
  format: "$time $level$levelpad $message $metadata\n",
  metadata: [:module, :function, :userid, :username], # default: []
  colors: [enabled: true],
  device: :user,
  max_buffer: 32
