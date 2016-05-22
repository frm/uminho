class Actor::Loader
  include TMDB
  SHOW_URI = '/person/:id'
  MOVIES_URI = '/person/:id/movie_credits'

  def self.find(id)
    response = TMDB.get(SHOW_URI, id: id)

    if response.success?
      Actor.new actor_params(response.parsed_response)
    else
      nil
    end
  end

  def self.actor_params(params)
    params.deep_symbolize_keys.slice(:name, :id)
  end

  def self.find_movies(id)
    response = TMDB.get(MOVIES_URI, id: id)

    if response.success?
      response.parsed_response['cast'].map { |m| Movie.new Movie::Loader.movie_params(m) }
    else
      []
    end
  end
end
