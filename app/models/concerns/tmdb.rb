module TMDB
  URL_PREFIX  = "https://api.themoviedb.org/3"
  API_KEY = ENV['TMDB_KEY']

  def self.get(uri, params = {})
    HTTParty.get(build_url(uri, params),
                 headers: headers)
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
