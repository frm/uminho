class Recipe::Searcher
  include CookNoobs
  INDEX_URI = '/export_imdb_app'

  def self.all
    response = CookNoobs.get(INDEX_URI)

    if response
      JSON.parse(response).map { |r| Recipe.new recipe_params(r) }
    else
      []
    end
  end

  private

  def self.recipe_params(params)
    symbolized_params = params.deep_symbolize_keys
    symbolized_params[:name] = symbolized_params[:receita]
    symbolized_params[:url] = symbolized_params[:urlVideo]
    symbolized_params[:score] = symbolized_params[:classificacao]

    symbolized_params.slice(:name, :url, :score)
  end
end
