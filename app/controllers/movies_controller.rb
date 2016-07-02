class MoviesController < ApplicationController
  def show
    @movie = Movie.find(params[:id])
    @actors = @movie.cast
    if request.xhr?
      render partial: 'movies/reviews'
    end
  end

  def trending
    @movies = Movie.trending
    render layout: 'movies'
  end

  def popular
    @movies = Movie.popular
    render layout: 'movies'
  end

  def releases
    @movies = Movie.releases
    render layout: 'movies'
  end

  def upcoming
    @movies = Movie.upcoming
    render layout: 'movies'
  end
end
