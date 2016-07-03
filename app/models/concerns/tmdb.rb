module TMDB
  URL_PREFIX = "https://api.themoviedb.org/3"
  API_KEY = ENV['TMDB_KEY']

  def self.get(uri, params = {})
    endpoint = build_url(uri, params)

    APICache.get(endpoint, cache: 3600, period: 0, timeout: 60) do
      response = HTTParty.get(endpoint, headers: headers)
      if response.success?
        response.parsed_response
      else
        raise APICache::InvalidResponse
      end
    end
  end

  def self.build_url(uri, params = {})
    new_uri = uri.dup

    params.each do |k,v|
      new_uri.gsub!(':' + k.to_s, v)
    end

    res = URL_PREFIX + new_uri + "?api_key=#{API_KEY}"
    params[:query] ? res + "&query=#{params[:query]}" : res
  end

  def self.headers(params = {})
    {
      "accept" => "application/json"
    }.merge(params)
  end
end
