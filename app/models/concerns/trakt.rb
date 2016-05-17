module Trakt
  API_VERSION = '2'
  URL_PREFIX  = "https://api-v2launch.trakt.tv"

  def self.get(uri, params = {})
    HTTParty.get(build_url(uri, params),
                 headers: headers)
  end

  def self.build_url(uri, params = {})
    new_uri = uri.dup

    params.each do |k,v|
      new_uri.gsub!(':' + k.to_s, v)
    end

    URL_PREFIX + new_uri
  end

  def self.headers(params = {})
    {
      "Content-Type" => "application/json",
      "trakt-api-version" => API_VERSION,
      "trakt-api-key" => ENV['TRAKT_ID']
    }.merge(params)
  end
end
