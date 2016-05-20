class Movie::TraktLoader
  include Trakt
  SHOW_URI    = '/movies/:id'
  INDEX_URI   = '/movies/trending'

  def self.find(id)
    response = Trakt.get(SHOW_URI, id: id)

    if response.success?
      Movie.new movie_params(response.parsed_response)
    else
      nil
    end
  end

  def self.all
    response = Trakt.get INDEX_URI

    # TODO: Trakt API allows for watchers when listing trending. Maybe allow
    # that in our view?
    # If so, movie_params must be adapted and called upon m instead of
    # m['movie']
    if response.success?
      response.parsed_response.map { |m| Movie.new movie_params(m['movie']) }
    else
      []
    end
  end

  def self.movie_params(params)
    symbolized_params = params.deep_symbolize_keys
    symbolized_params[:id] = symbolized_params[:ids][:trakt]
    symbolized_params.slice(:title, :year, :id)
  end
end
