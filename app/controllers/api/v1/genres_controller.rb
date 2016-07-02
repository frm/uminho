class Api::V1::GenresController < ApplicationController
  def show
    genres = Genre.all

    if(params[:family])
      @movies = genres.select { |g| g.name == "Family" }.first.movies
    else
      id = params[:id].to_i % genres.length
      @movies = genres[id].movies
    end

    #ActiveModel::Serializer::ArraySerializer.new(@movies, each_serializer: MovieSerializer).to_json
    render json: @movies
  end
end
