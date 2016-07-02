class Movie::Loader
  include TMDB
  SHOW_URI      = '/movie/:id'
  TRENDING_URI  = '/movie/popular'
  UPCOMING_URI  = '/movie/upcoming'
  RELEASES_URI  = '/movie/now_playing'
  CAST_URI      = '/movie/:id/credits'
  IMG_PATH      = 'https://image.tmdb.org/t/p/w396'

  def self.find(id)
    response = TMDB.get(SHOW_URI, id: id)

    if response.success?
      m = Movie.new movie_params(response.parsed_response)
      m.cache_genres(response["genres"].map { |g| g["id"] })
      m
    else
      nil
    end
  end

  def self.trending
    get_collection TRENDING_URI
  end

  def self.upcoming
    get_collection UPCOMING_URI
  end

  def self.releases
    get_collection RELEASES_URI
  end

  def self.find_cast(id)
    response = TMDB.get(CAST_URI, id: id)

    if response.success?
      res = response.parsed_response
      [
        res['cast'].map { |a| Actor.new Actor::Loader.actor_params(a) },
        res['crew'].select { |c| c["job"] == "Director" }
                    .map { |c| Actor.new Actor::Loader.actor_params(c) }
      ]
    else
      []
    end
  end

  def self.movie_params(params)
    symbolized_params = params.deep_symbolize_keys

    if symbolized_params[:release_date]
      symbolized_params[:year] =
        symbolized_params[:release_date].split('-').first.to_i
    end

    if symbolized_params[:poster_path]
      symbolized_params[:img_path] = IMG_PATH + symbolized_params[:poster_path]
    end

    if symbolized_params[:spoken_languages]
      symbolized_params[:language] = retrieve_language(symbolized_params)
    end

    symbolized_params.slice(:title, :year, :id, :img_path, :overview,
                           :tagline, :runtime, :language)
  end

  def self.retrieve_language(params)
    return "Unknown" unless params[:spoken_languages]

    language = params[:spoken_languages].select do |lang|
      lang[:iso_639_1] == params[:original_language]
    end.first

    language.nil? ? "Unknown" : language[:name]
  end

  private

  def self.get_collection(uri)
    response = TMDB.get uri
    if response.success?
      response.parsed_response["results"].map { |m| Movie.new movie_params(m) }
    else
      []
    end
  end
end
