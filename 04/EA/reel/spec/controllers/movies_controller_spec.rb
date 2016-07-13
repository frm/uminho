require 'rails_helper'

RSpec.describe MoviesController, type: :controller do
  login_user

  before do
    @movies = FactoryGirl.create_list :movie, 20
    @movie = FactoryGirl.create :movie
    @another_movie = FactoryGirl.create :movie
    allow(Movie).to receive(:popular).and_return(@movies)
    allow(Movie).to receive(:find).and_return(@movie, @another_movie)
  end

  describe "GET #popular" do
    it "correctly assigns @movies" do
      get :popular
      expect(assigns(:movies)).to eq(@movies)
    end

    it "renders the popular template" do
      get :popular
      expect(response).to render_template("popular")
    end
  end

  describe "GET #show" do
    it "correctly assigns @movie" do
      get :show, id: @movie.id
      expect(assigns(:movie)).to eq(@movie)
    end

    it "renders the show template" do
      get :show, id: @another_movie
      expect(response).to render_template("show")
    end
  end
end
