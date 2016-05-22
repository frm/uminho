class Movie::Loader
  include TMDB
  SHOW_URI    = '/movie/:id'
  INDEX_URI   = '/movie/popular'

  def self.find(id)
    response = TMDB.get(SHOW_URI, id: id)

    if response.success?
      Movie.new movie_params(response.parsed_response)
    else
      nil
    end
  end

  def self.all
    response = TMDB.get INDEX_URI

    # TODO: Trakt API allows for watchers when listing trending. Maybe allow
    # that in our view?
    # If so, movie_params must be adapted and called upon m instead of
    # m['movie']
    if response.success?
      response.parsed_response["results"].map { |m| Movie.new movie_params(m) }
    else
      []
    end
  end

  def self.movie_params(params)
    symbolized_params = params.deep_symbolize_keys
    symbolized_params[:year] =
      symbolized_params[:release_date].split('-').first.to_i
    symbolized_params.slice(:title, :year, :id)
  end
end
