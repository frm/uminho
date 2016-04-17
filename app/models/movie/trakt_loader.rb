class Movie::TraktLoader
  API_VERSION = '2'
  URL_PREFIX = "https://api-v2launch.trakt.tv"
  SHOW_URI = '/movies/:id'
  INDEX_URI = '/movies/trending'

  def self.find(id)
    response = get(SHOW_URI, id: id)

    if response.success?
      Movie.new sanitize_params(response.parsed_response)
    else
      nil
    end
  end

  def self.all
    response = get(INDEX_URI)

    # TODO: Trakt API allows for watchers when listing trending. Maybe allow
    # that in our view?
    # If so, sanitize_params must be adapted and called upon m instead of
    # m['movie']
    if response.success?
      response.parsed_response.map { |m| Movie.new sanitize_params(m['movie']) }
    else
      nil
    end
  end

  private

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

  def self.sanitize_params(params)
    symbolized_params = params.deep_symbolize_keys
    symbolized_params[:id] = symbolized_params[:ids][:trakt]
    symbolized_params.slice(:title, :year, :id)
  end

  def self.headers
    {
      "Content-Type" => "application/json",
      "trakt-api-version" => API_VERSION,
      "trakt-api-key" => ENV['TRAKT_ID']
    }
  end
end
