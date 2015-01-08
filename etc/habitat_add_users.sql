USE habitat;

-- Creating test root user
CREATE USER 'habitat'@'127.0.0.1' IDENTIFIED BY 'testuser123';
GRANT ALL ON habitat.* TO 'habitat'@'127.0.0.1';

DROP USER 'habitat'@'127.0.0.1';
