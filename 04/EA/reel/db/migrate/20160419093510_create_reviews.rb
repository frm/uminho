class CreateReviews < ActiveRecord::Migration
  def change
    create_table :reviews do |t|
      t.float :score
      t.string :description
      t.references :user
      t.references :movie

      t.timestamps null: false
    end
  end
end
