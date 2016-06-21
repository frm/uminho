class Movie::Loader
  include TMDB
  SHOW_URI    = '/movie/:id'
  INDEX_URI   = '/movie/popular'
  CAST_URI    = '/movie/:id/credits'
  IMG_PATH    = 'https://image.tmdb.org/t/p/w396'

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
    if response.success?
      response.parsed_response["results"].map { |m| Movie.new movie_params(m) }
    else
      []
    end
  end

  def self.find_cast(id)
    response = TMDB.get(CAST_URI, id: id)

    if response.success?
      response.parsed_response['cast'].map { |a| Actor.new Actor::Loader.actor_params(a) }
    else
      []
    end
  end

  def self.movie_params(params)
    symbolized_params = params.deep_symbolize_keys
    symbolized_params[:year] =
      symbolized_params[:release_date].split('-').first.to_i

    symbolized_params[:img_path] = IMG_PATH + symbolized_params[:poster_path]
    symbolized_params.slice(:title, :year, :id, :img_path, :overview, :tagline)
  end
end
