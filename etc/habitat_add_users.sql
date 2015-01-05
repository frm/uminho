USE `habitat`;

-- Creating test root user
CREATE USER 'habitat'@'localhost' IDENTIFIED BY 'testuser123';
GRANT ALL ON habitat.* TO 'habitat'@'localhost';

