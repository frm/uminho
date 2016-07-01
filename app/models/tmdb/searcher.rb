module TMDB::Searcher
  include TMDB
  QUERY_URI = '/search/multi'
  PERSON_URI = '/search/person'

  def self.search(query)
    response = TMDB.get(QUERY_URI, query: query)

    if response.success?
      results = response.parsed_response["results"]
      parse_search_results results
    else
      {}
    end
  end

  def self.actor_search(query)
    response = TMDB.get(PERSON_URI, query: query)

    if response.success?
      results = response.parsed_response["results"]
      results.map { |a| Actor.new Actor::Loader.actor_params(a) }
    else
      {}
    end

  end

  private

  def self.parse_search_results(res)
    res.reduce(Hash.new { |h, k| h[k] = Array.new }, &method(:parse_media))
  end

  def self.parse_media(acc, media)
    case media["media_type"]
    when "movie"
      acc[:movies].push(Movie.new Movie::Loader.movie_params media)
      acc
    when "person"
      acc[:actors].push(Actor.new Actor::Loader.actor_params media)
      acc
    else
      acc
    end
  end
end
