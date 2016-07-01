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

  def actor
    @movies = []
    @actors = []
    params[:actors].each do |a|
      actor = Actor.find(a)
      @actors << actor
      actor.movies.each do |m|
        @movies << m if m.genres.include? params[:genre_id]
      end
    end

    @genres = Genre.all
    @genre = @genres.select{ |g| g.id == params[:genre_id].to_i }.first

    render :show
  end
end
