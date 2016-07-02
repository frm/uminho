require 'rails_helper'

RSpec.describe SearchController, type: :controller do
  login_user

  describe "GET #search" do
    describe "for a valid actor" do
      before(:each) do
        @actor = FactoryGirl.create :actor
        allow(TMDB::Searcher).to receive(:search).and_return({actors: [@actor]})
      end

      it "includes @actor in the results" do
        get :index, q: @actor.name
        expect(assigns(:results)[:actors]).to include(@actor)
      end

      it "renders the index template" do
        get :index, q: @actor.name
        expect(response).to render_template(:index)
      end
    end

    describe "for an invalid actor" do
      before(:each) do
        @actor = FactoryGirl.create :actor
        allow(TMDB::Searcher).to receive(:search).and_return({actors: []})
      end

      it "doesn't include @actor in the results" do
        get :index, q: @actor.name
        expect(assigns(:results)[:actors]).not_to include(@actor)
      end
    end

    describe "for a valid movie" do
      before(:each) do
        @movie = FactoryGirl.create :movie
        allow(TMDB::Searcher).to receive(:search).and_return({movies: [@movie]})
      end

      it "includes @movie in the results" do
        get :index, q: @movie.title
        expect(assigns(:results)[:movies]).to include(@movie)
      end
    end

    describe "for an invalid movie" do
      before(:each) do
        @movie = FactoryGirl.create :movie
        allow(TMDB::Searcher).to receive(:search).and_return({movies: []})
      end

      it "includes @movie in the results" do
        get :index, q: @movie.title
        expect(assigns(:results)[:movies]).not_to include(@movie)
      end
    end

    describe "user search" do
      before(:each) do
        @user = FactoryGirl.create :user
        allow(TMDB::Searcher).to receive(:search).and_return({})
      end

      context "when searching by an existing name" do
        it "contains the requested user in the results" do
          get :index, q: @user.name
          expect(assigns(:results)[:users]).to include(@user)
        end
      end

      context "when searching for an inexistent name" do
        it "doesn't contain the requested user in the results" do
          another_user = FactoryGirl.build :user
          get :index, q: another_user.name
          expect(assigns(:results)[:users]).to be_nil
        end
      end

      context "when searching by an existing email" do
        it "contains the requested user in the results" do
          get :index, q: @user.email
          expect(assigns(:results)[:users]).to include(@user)
        end
      end

      context "when searching for an inexistent name" do
        it "doesn't contain the requested user in the results" do
          another_user = FactoryGirl.build :user
          get :index, q: another_user.email
          expect(assigns(:results)[:users]).to be_nil
        end
      end
    end
  end
end
