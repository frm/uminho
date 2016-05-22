class Genre::Loader
  include TMDB
  SHOW_URI    = '/genre/:id/movies'
  INDEX_URI   = '/genre/movie/list'

  def self.movies(id)
    response = TMDB.get(SHOW_URI, id: id)

    if response.success?
      response.parsed_response["results"].map { |m| Movie.new Movie::Loader.movie_params(m) }
    else
      []
    end
  end

  def self.all
    response = TMDB.get INDEX_URI
    if response.success?
      response.parsed_response["genres"].map { |g| Genre.new genre_params(g) }
    else
      []
    end
  end

  def self.genre_params(params)
    params.deep_symbolize_keys.slice(:id, :name)
  end
end
