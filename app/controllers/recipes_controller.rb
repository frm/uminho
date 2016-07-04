class RecipesController < ApplicationController
  def index
    @recipes = Recipe.all.last(20)
  end
end
