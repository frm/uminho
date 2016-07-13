class MovieSerializer < ActiveModel::Serializer
  attributes :id, :title, :year, :img_path, :overview, :genres

  def genres
    object.genre_names
  end
end
