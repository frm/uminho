class Actor::Loader
  include TMDB
  SHOW_URI = '/person/:id'
  MOVIES_URI = '/person/:id/movie_credits'
  IMG_PATH = 'https://image.tmdb.org/t/p/w396'
  IMG_PLACEHOLDER = 'http://www.clker.com/cliparts/A/Y/O/m/o/N/placeholder.svg'

  def self.find(id)
    response = TMDB.get(SHOW_URI, id: id)

    if response.success?
      Actor.new actor_params(response.parsed_response)
    else
      nil
    end
  end

  def self.actor_params(params)
    symbolized_params = params.deep_symbolize_keys

    profile_path = symbolized_params[:profile_path]
    symbolized_params[:profile_path] =
      profile_path ? IMG_PATH + profile_path : IMG_PLACEHOLDER

    symbolized_params.slice(:name, :id, :profile_path, :character,
                           :biography, :homepage, :place_of_birth, :birthday)
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
