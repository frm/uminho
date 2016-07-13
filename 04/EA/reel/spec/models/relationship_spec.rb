require 'rails_helper'

RSpec.describe Relationship, type: :model do
  before do
    @relationship = Relationship.new(follower_id: 1, followed_id: 2)
  end

  subject { @relationship }

  describe "attributes" do
    it "should require a follower id" do
      @relationship.follower_id = nil
      expect(@relationship).not_to be_valid
    end

    it "should require a followed id" do
      @relationship.followed_id = nil
      expect(@relationship).not_to be_valid
    end
  end
end
