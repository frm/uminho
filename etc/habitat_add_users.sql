USE `habitat`;

-- Creating test root user
CREATE USER 'habitat_admin'@'localhost' IDENTIFIED BY 'testuser123';
GRANT ALL ON habitat.* TO 'habitat_admin'@'localhost';

