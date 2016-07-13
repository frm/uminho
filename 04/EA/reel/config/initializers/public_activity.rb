if defined?(Rails::Console) || Rails.env == "test"
  PublicActivity.enabled = false
end
