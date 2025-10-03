---------------------------
-- MySQL Schema (run once)
---------------------------
-- # Save the following as schema.sql and run it on your MySQL instance.

CREATE TABLE IF NOT EXISTS students (
id BIGINT AUTO_INCREMENT PRIMARY KEY,
first_name VARCHAR(100) NOT NULL,
last_name VARCHAR(100) NOT NULL,
phone VARCHAR(25) NULL,
email VARCHAR(255) NULL UNIQUE,
grad_month TINYINT NULL,
grad_year SMALLINT NULL,
hometown VARCHAR(255) NULL,
major VARCHAR(120) NULL,
linkedin_url VARCHAR(255) NULL,
instagram_handle VARCHAR(120) NULL,
created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
INDEX idx_students_name (last_name, first_name),
INDEX idx_students_major (major),
INDEX idx_students_grad (grad_year, grad_month)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS interactions (
id BIGINT AUTO_INCREMENT PRIMARY KEY,
student_id BIGINT NOT NULL,
occurred_at DATETIME NOT NULL,
place VARCHAR(120) NULL,
notes TEXT NOT NULL,
created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
INDEX idx_interactions_student_time (student_id, occurred_at),
CONSTRAINT fk_interactions_student FOREIGN KEY (student_id) REFERENCES students(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Optional: seed a test student
INSERT INTO students (first_name, last_name, email, major, grad_month, grad_year)
VALUES ('Test','Student','test@example.com','Supply Chain',12,2026);
