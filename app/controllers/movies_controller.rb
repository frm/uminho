class MoviesController < ApplicationController
  def show
    @movie = Movie::TraktLoader.find(params[:id])
    @actors = Movie::TraktLoader.movie_cast(params[:id])
  end

  def index
    @movies = Movie::TraktLoader.all
  end
end
