# ⚾ Baseball Elo Tracker

A full-stack web application that allows users to track MLB teams' Elo ratings, predict game outcomes, batting/pitching/fielding statistics, comment on team performance, and share blog-style analysis with the community.

Built as a final project for S428 (Spring 2025), this project demonstrates skills in Flask, SQL, HTML/CSS, user authentication, and dynamic web design.

---

## 🚀 Features

- ✅ Team Elo rating display with batting, pitching, and fielding statistics
- ✅ Dynamic team pages with user comments and stats tables
- ✅ User authentication system (register, login, logout, profile management)
- ✅ Blog post functionality with comment threads
- ✅ Messaging system (user-to-user)
- ✅ Responsive frontend using HTML & CSS
- ✅ Secure backend with input validation and session management
- ✅ SQLite database integration and normalized schema

---

## 🛠️ Technology Stack

- **Backend:** Python, Flask, SQLite, Jinja2
- **Frontend:** HTML5, CSS3, Bootstrap (optional)
- **Authentication:** Flask-Login, Flask-Bcrypt
- **Database:** SQLite3
- **Testing:** `unittest`, `pytest` (bonus: Selenium)

---

## 📦 Setup Instructions

### 1. Install requirements
pip install -r requirements.txt

### 2. Run R-Scripts
Rscript 01_scrape_data.R
Rscript 02_team_elo_training.R
Rscript 03_team_elo_rating.R
Rscript 05_predict_game_outcomes.R
Rscript 06_xgboost_analysis.R
Rscript 07_model_analysis.R
Rscript 08_export_data_plots.R
Rscript 09_export_visualizations.R

### 3. Initialize Database
python DatabaseSetUpScripts/temp.py

### 4. Import Teams & Team Stats
python DatabaseSetUpScripts/import_teams.py
python DatabaseSetUpScripts/import_team_stats.py

### 5. Run Flask
python app.py

For questions or collaboration, reach out via [misutker@iu.edu] or open an issue in the repo.
