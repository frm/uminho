class MoviesController < ApplicationController
  def show
    @movie = Movie.find(params[:id])
    @actors = @movie.cast
    if request.xhr?
      render partial: 'movies/reviews'
    end
  end

  def index
    @movies = Movie.trending
  end
end
