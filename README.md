# ⚾ Baseball Elo Tracker

A full-stack web application that allows users to track MLB teams' Elo ratings, analyze batting, pitching, and fielding statistics, and predict game outcomes. The project integrates Python and R for data processing, modeling, and visualization, showcasing advanced skills in web development, data science, and machine learning.

Built as a final project for S428 (Spring 2025), this application demonstrates expertise in Flask, SQL, R programming, and dynamic web design.

---

## 🚀 Features

- ✅ Team Elo rating display with detailed batting, pitching, and fielding statistics
- ✅ Dynamic team pages with user comments, stats tables, and visualizations
- ✅ User authentication system (register, login, logout, profile management)
- ✅ Blog post functionality with comment threads
- ✅ Messaging system (user-to-user)
- ✅ Predictive modeling for game outcomes using machine learning
- ✅ Responsive frontend using HTML & CSS
- ✅ Secure backend with input validation and session management
- ✅ SQLite database integration and normalized schema
- ✅ Data visualization and animation using R (e.g., ggplot2, gganimate)

---

## 🛠️ Technology Stack

- **Backend:** Python, Flask, SQLite, Jinja2
- **Frontend:** HTML5, CSS3, Bootstrap (optional)
- **Authentication:** Flask-Login, Flask-Bcrypt
- **Database:** SQLite3
- **Data Science:** R (baseballr, ggplot2, randomForest, xgboost)
- **Testing:** `unittest`, `pytest` (bonus: Selenium)

---

## 📦 Setup Instructions

### 1. Install Python dependencies
```bash
pip install -r requirements.txt
```

### 2. Install R dependencies
Ensure you have R installed, then run:
```R
install.packages(c("baseballr", "dplyr", "readr", "lubridate", "ggplot2", "ggrepel", "gganimate", "scales", "tidyr", "randomForest", "gbm", "tibble", "SHAPforxgboost", "xgboost", "caret", "Matrix", "pROC", "car", "corrplot"))
```

### 3. Populate the `/data` folder
Run the R script to generate necessary data files:
```bash
Rscript 01_scrape_data.r
Rscript 02_team_elo_training.r
Rscript 03_team_elo_rating.R
Rscript 05_predict_game_outcomes.R
Rscript 06_xgboost_predictions.R
Rscript 07_model_analysis.R
Rscript 08_export_data_plots.R
Rscript 09_export_visualizations.R
Rscript 10_TeamEloPlot.r
```

### 4. Initialize the database
Run the following Python script to set up the SQLite database:
```bash
python DatabaseSetUpScript/01_temp.py


### 5. Import teams and team statistics
Run these scripts sequentially:
```bash
python DatabaseSetUpScript/02_import_teams.py
python DatabaseSetUpScript/03_import_team_stats.py
```

### 6. Start the Flask application
Launch the web application:
```bash
python app.py
```

### 7. Access the application
Open your browser and navigate to `http://127.0.0.1:5000` to use the Baseball Elo Tracker.

---

For questions or collaboration, reach out via [misutker@iu.edu] or open an issue in the repo.
