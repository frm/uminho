class GenresController < ApplicationController
  layout "sidebar"

  def show
    @genres = Genre.all
    @genre = @genres.select{ |g| g.id == params[:id].to_i }.first
  end

  def index
    @genres = Genre.all
    @movies = Movie.trending
  end
end
