from flask import Flask, render_template, request, redirect, url_for, session, flash, abort
from flask_bcrypt import Bcrypt
import sqlite3
from datetime import datetime
import os
from werkzeug.utils import secure_filename
import math
import pandas as pd
from functools import wraps
import re

app = Flask(__name__)
app.secret_key = 'your_secret_key_here'
bcrypt = Bcrypt(app)

app.config['UPLOAD_FOLDER'] = 'static/images'
ALLOWED_EXTENSIONS = {'png', 'jpg', 'jpeg', 'gif'}

# Form validation utilities
def validate_email(email):
    """Validate email format using regex"""
    pattern = r'^[\w\.-]+@[\w\.-]+\.\w+$'
    return bool(re.match(pattern, email))

def validate_password_strength(password):
    """Check if password meets minimum strength requirements"""
    # At least 8 characters
    if len(password) < 8:
        return False, "Password must be at least 8 characters long"
    # Check for at least one digit
    if not re.search(r'\d', password):
        return False, "Password must contain at least one number"
    # Check for at least one uppercase letter
    if not re.search(r'[A-Z]', password):
        return False, "Password must contain at least one uppercase letter"
    # Check for at least one lowercase letter
    if not re.search(r'[a-z]', password):
        return False, "Password must contain at least one lowercase letter"
    # Check for at least one special character
    if not re.search(r'[!@#$%^&*(),.?":{}|<>]', password):
        return False, "Password must contain at least one special character"
    return True, "Password is strong"

def is_email_registered(email):
    """Check if email is already registered in database"""
    conn = get_db_connection()
    result = conn.execute('SELECT id FROM users WHERE email = ?', (email,)).fetchone()
    conn.close()
    return result is not None

# Load team data
batting_data_2025 = pd.read_csv("data/TeamBattingStatistics2025.csv")
pitching_data_2025 = pd.read_csv("data/TeamPitchingStatistics2025.csv")
fielding_data_2025 = pd.read_csv("data/TeamFieldingStatistics2025.csv")

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

def get_db_connection():
    conn = sqlite3.connect('database.db')
    conn.row_factory = sqlite3.Row
    return conn

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/about')
def about():
    return render_template('about.html', title="About", description="About the Baseball Elo Project")

@app.route('/add', methods=['GET', 'POST'])
def add():
    if request.method == 'POST':
        name = request.form['name']
        email = request.form['email']
        password = request.form['password']
        
        # Form validation
        errors = []
        
        # Email validation
        if not email:
            errors.append("Email is required")
        elif not validate_email(email):
            errors.append("Invalid email format. Please use a valid email address.")
        elif is_email_registered(email):
            errors.append("This email is already registered. Please use a different email.")
            
        # Password validation
        if not password:
            errors.append("Password is required")
        else:
            is_valid, message = validate_password_strength(password)
            if not is_valid:
                errors.append(message)
        
        # If there are validation errors, show them and return to the form
        if errors:
            for error in errors:
                flash(error, 'error')
            return render_template('add.html')
        
        # Proceed with registration
        hashed_pw = bcrypt.generate_password_hash(password).decode('utf-8')

        image_file = request.files.get('image')
        image_filename = None
        if image_file and allowed_file(image_file.filename):
            image_filename = secure_filename(image_file.filename)
            image_file.save(os.path.join(app.config['UPLOAD_FOLDER'], image_filename))

        conn = get_db_connection()
        conn.execute('INSERT INTO users (name, email, password, image) VALUES (?, ?, ?, ?)',
                     (name, email, hashed_pw, image_filename))
        conn.commit()
        conn.close()
        flash('Account created successfully! Please log in.', 'success')
        return redirect(url_for('login'))
    return render_template('add.html')

@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'POST':
        email = request.form['email']
        password_input = request.form['password']
        
        # Form validation
        errors = []
        
        # Check for empty email
        if not email:
            errors.append("Email is required")
        
        # Check for empty password
        if not password_input:
            errors.append("Password is required")
        
        # If there are validation errors, show them and return to the form
        if errors:
            for error in errors:
                flash(error, 'error')
            return render_template('login.html')
        
        # Continue with login process
        conn = get_db_connection()
        user = conn.execute('SELECT * FROM users WHERE email = ?', (email,)).fetchone()
        conn.close()

        if user and bcrypt.check_password_hash(user['password'], password_input):
            session['user_id'] = user['id']
            session['user_name'] = user['name']
            session['user_role'] = user['role']
            flash('Login successful', 'success')
            return redirect(url_for('dashboard'))
        else:
            flash('Invalid email or password', 'error')
    return render_template('login.html')

def admin_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if session.get('user_role') != 'admin':
            abort(403)
        return f(*args, **kwargs)
    return decorated_function

@app.route('/admin')
@admin_required
def admin_dashboard():
    conn = get_db_connection()  # ✅ Add this line
    users = conn.execute("SELECT * FROM users").fetchall()
    return render_template("admin_dashboard.html", users=users)

@app.route('/logout')
def logout():
    session.clear()
    flash('You have been logged out.', 'info')
    return redirect(url_for('index'))

@app.route('/dashboard', methods=['GET', 'POST'])
def dashboard():
    if 'user_id' not in session:
        flash("Please log in to access the dashboard.", "error")
        return redirect(url_for('login'))

    user_id = session['user_id']
    reply_to = request.args.get('reply_to', type=int)
    edit_id = request.args.get('edit_id', type=int)
    thread_id = request.args.get('thread', type=int)

    conn = get_db_connection()

    if request.method == 'POST':
        content = request.form['message']
        
        # Validate message content is not empty
        if not content or content.strip() == "":
            flash("Message content cannot be empty.", "error")
            
            # Preserve the context for the form
            if 'edit_id' in request.form:
                return redirect(url_for('dashboard', edit_id=request.form['edit_id']))
            elif 'reply_to' in request.form:
                return redirect(url_for('dashboard', reply_to=request.form['reply_to']))
            else:
                return redirect(url_for('dashboard'))
        
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        image_file = request.files.get('image')
        image_filename = None

        if image_file and allowed_file(image_file.filename):
            image_filename = secure_filename(image_file.filename)
            image_file.save(os.path.join(app.config['UPLOAD_FOLDER'], image_filename))

        if 'edit_id' in request.form:
            message_id = request.form['edit_id']
            # Get existing message to check ownership
            existing = conn.execute('SELECT * FROM messages WHERE id = ?', (message_id,)).fetchone()
            if existing and existing['user_id'] == user_id:
                if image_file and image_filename:
                    # Delete old image if it exists
                    if existing['image']:
                        try:
                            os.remove(os.path.join(app.config['UPLOAD_FOLDER'], existing['image']))
                        except:
                            pass
                else:
                    image_filename = existing['image']  # Keep existing image if no new one
                
                conn.execute('UPDATE messages SET content = ?, timestamp = ?, image = ? WHERE id = ?',
                            (content, timestamp, image_filename, message_id))
                flash('Message updated successfully', 'success')
                
                # Get thread_id either from form or from parent_id
                redirect_thread_id = request.form.get('thread_id', type=int)
                if not redirect_thread_id and existing['parent_id']:
                    redirect_thread_id = existing['parent_id']

                if redirect_thread_id:
                    return redirect(url_for('dashboard', thread=redirect_thread_id))
                return redirect(url_for('dashboard'))
        else:
            parent_id = request.form.get('reply_to')
            parent_id = int(parent_id) if parent_id else None
            
            conn.execute('''
                INSERT INTO messages (user_id, content, timestamp, parent_id, image) 
                VALUES (?, ?, ?, ?, ?)
            ''', (user_id, content, timestamp, parent_id, image_filename))
            flash('Message posted successfully', 'success')
        
        conn.commit()
        
        # Redirect to thread view if replying to a thread
        if parent_id:
            return redirect(url_for('dashboard', thread=parent_id))
        return redirect(url_for('dashboard'))

    if thread_id:
        # Get main thread and its replies
        messages = conn.execute('''
            SELECT m.*, u.name, u.image as user_image 
            FROM messages m
            JOIN users u ON m.user_id = u.id
            WHERE m.id = ? OR m.parent_id = ?
            ORDER BY m.timestamp ASC
        ''', (thread_id, thread_id)).fetchall()
    else:
        # Get only main threads (no replies)
        messages = conn.execute('''
            SELECT m.*, u.name, u.image as user_image,
                   (SELECT COUNT(*) FROM messages r WHERE r.parent_id = m.id) as reply_count
            FROM messages m
            JOIN users u ON m.user_id = u.id
            WHERE m.parent_id IS NULL
            ORDER BY m.timestamp DESC
        ''').fetchall()

    conn.close()
    return render_template('dashboard.html', 
                         messages=messages, 
                         reply_to=reply_to, 
                         edit_id=edit_id,
                         thread_id=thread_id)

@app.route('/delete_message/<int:message_id>', methods=['POST'])
def delete_message(message_id):
    if 'user_id' not in session:
        flash("Please log in to delete messages.", "error")
        return redirect(url_for('login'))

    conn = get_db_connection()
    message = conn.execute('SELECT * FROM messages WHERE id = ?', (message_id,)).fetchone()
    
    if not message or message['user_id'] != session['user_id']:
        flash("You can only delete your own messages.", "error")
        conn.close()
        return redirect(url_for('dashboard'))

    # Delete the message's image if it exists
    if message['image']:
        try:
            os.remove(os.path.join(app.config['UPLOAD_FOLDER'], message['image']))
        except:
            pass

    # Delete the message and all its replies
    conn.execute('DELETE FROM messages WHERE id = ? OR parent_id = ?', (message_id, message_id))
    conn.commit()
    conn.close()

    flash("Message deleted successfully.", "success")
    return redirect(url_for('dashboard'))

@app.route('/edit_user/<int:user_id>', methods=['GET', 'POST'])
@admin_required
def edit_user(user_id):
    conn = get_db_connection()
    user = conn.execute('SELECT * FROM users WHERE id = ?', (user_id,)).fetchone()

    if not user:
        conn.close()
        flash("User not found.", "error")
        return redirect(url_for('admin_dashboard'))

    if request.method == 'POST':
        name = request.form['name']
        email = request.form['email']
        role = request.form['role']

        conn.execute('UPDATE users SET name = ?, email = ?, role = ? WHERE id = ?',
                     (name, email, role, user_id))
        conn.commit()
        conn.close()
        flash("User updated successfully.", "success")
        return redirect(url_for('admin_dashboard'))

    conn.close()
    return render_template('edit_user.html', user=user)

@app.route('/delete_user/<int:user_id>', methods=['POST'])
@admin_required
def delete_user(user_id):
    # Prevent admin from deleting their own account
    if session.get('user_id') == user_id:
        flash("❌ You cannot delete your own account while logged in.", "error")
        return redirect(url_for('admin_dashboard'))

    conn = get_db_connection()
    conn.execute("DELETE FROM users WHERE id = ?", (user_id,))
    conn.commit()
    conn.close()

    flash("✅ User deleted successfully.", "success")
    return redirect(url_for('admin_dashboard'))

@app.route('/edit/<int:note_id>', methods=['GET', 'POST'])
def edit(note_id):
    if 'user_id' not in session:
        return redirect(url_for('login'))

    conn = get_db_connection()
    note = conn.execute('SELECT * FROM notes WHERE id = ? AND user_id = ?', 
                        (note_id, session['user_id'])).fetchone()

    if not note:
        conn.close()
        flash("Note not found or unauthorized.", "error")
        return redirect(url_for('dashboard'))

    if request.method == 'POST':
        updated_note = request.form['note']
        conn.execute('UPDATE notes SET content = ?, timestamp = ? WHERE id = ?',
                     (updated_note, datetime.now().strftime("%Y-%m-%d %H:%M:%S"), note_id))
        conn.commit()
        conn.close()
        return redirect(url_for('dashboard'))

    conn.close()
    return render_template('edit.html', note=note)

@app.route('/delete/<int:note_id>', methods=['POST'])
def delete(note_id):
    if 'user_id' not in session:
        return redirect(url_for('login'))

    conn = get_db_connection()
    conn.execute('DELETE FROM notes WHERE id = ? AND user_id = ?', 
                 (note_id, session['user_id']))
    conn.commit()
    conn.close()
    return redirect(url_for('dashboard'))

@app.route('/teams')
def teams():
    conn = get_db_connection()
    teams = conn.execute('''
        SELECT t.*, ta.abbreviation as team_abbrev 
        FROM teams t
        LEFT JOIN team_abbr_lookup ta ON t.team_name = ta.team_name
        ORDER BY t.final_elo DESC
    ''').fetchall()
    conn.close()
    return render_template('teams.html', teams=teams)

@app.route('/team_batting')
def team_batting():
    df = pd.read_csv('data/TeamBattingStatistics2025.csv')
    df = df.round(3)
    selected_team = request.args.get('team', '')

    if selected_team:
        df = df[df['team_name'] == selected_team]

    team_list = sorted(df['team_name'].unique())
    columns = df.columns.tolist()
    data = df.to_dict(orient='records')

    return render_template('team_batting.html', columns=columns, data=data, team_list=team_list, selected_team=selected_team)

@app.route('/team_pitching')
def team_pitching():
    df = pd.read_csv('data/TeamPitchingStatistics2025.csv')
    df = df.round(3)
    selected_team = request.args.get('team', '')

    if selected_team:
        df = df[df['team_name'] == selected_team]

    team_list = sorted(df['team_name'].unique())
    columns = df.columns.tolist()
    data = df.to_dict(orient='records')

    return render_template('team_pitching.html', columns=columns, data=data, team_list=team_list, selected_team=selected_team)

@app.route('/team_fielding')
def team_fielding():
    df = pd.read_csv('data/TeamFieldingStatistics2025.csv')
    df = df.round(3)
    selected_team = request.args.get('team', '')

    if selected_team:
        df = df[df['team_name'] == selected_team]

    team_list = sorted(df['team_name'].unique())
    columns = df.columns.tolist()
    data = df.to_dict(orient='records')

    return render_template('team_fielding.html', columns=columns, data=data, team_list=team_list, selected_team=selected_team)

@app.route('/teams/<int:team_id>', methods=['GET', 'POST'])
def team_detail(team_id):
    conn = get_db_connection()
    team = conn.execute('SELECT * FROM teams WHERE team_id = ?', (team_id,)).fetchone()

    if not team:
        conn.close()
        return "Team not found", 404

    # Pull extra stats using correct team name format
    short_name = team['team_name']
    abbr_row = conn.execute("SELECT abbreviation FROM team_abbr_lookup WHERE team_name = ?", (short_name,)).fetchone()
    abbreviation = abbr_row['abbreviation'] if abbr_row else None
    short = short_name.split()[-1]

    # Get stat rows
    batting_row = conn.execute("SELECT * FROM team_batting WHERE team_name = ?", (short,)).fetchone()
    batting = dict(batting_row) if batting_row else None

    pitching_row = conn.execute("SELECT * FROM team_pitching WHERE team_name = ?", (short,)).fetchone()
    pitching = dict(pitching_row) if pitching_row else None

    fielding_row = conn.execute("SELECT * FROM team_fielding WHERE team_name = ?", (short,)).fetchone()
    fielding = dict(fielding_row) if fielding_row else None

    # Handle comment editing, replying, and new comments
    reply_to = request.args.get('reply_to', type=int)
    edit_id = request.args.get('edit_id', type=int)
    edit_comment = None
    reply_author = None

    if edit_id:
        edit_comment = conn.execute('''
            SELECT r.*, u.name 
            FROM reviews r 
            JOIN users u ON r.user_id = u.id 
            WHERE r.id = ? AND r.user_id = ?
        ''', (edit_id, session.get('user_id'))).fetchone()

    if reply_to:
        reply_author_row = conn.execute('''
            SELECT u.name 
            FROM reviews r 
            JOIN users u ON r.user_id = u.id 
            WHERE r.id = ?
        ''', (reply_to,)).fetchone()
        reply_author = reply_author_row['name'] if reply_author_row else None

    if request.method == 'POST':
        if 'user_id' not in session:
            flash("You must be logged in to comment.", "error")
            return redirect(url_for('login'))

        comment = request.form['comment']
        
        # Validate that comment text is not blank
        if not comment or comment.strip() == "":
            flash("Comment cannot be empty.", "error")
            
            # Preserve the context for the form
            if 'edit_id' in request.form:
                return redirect(url_for('team_detail', team_id=team_id, edit_id=request.form['edit_id']))
            elif 'parent_id' in request.form:
                return redirect(url_for('team_detail', team_id=team_id, reply_to=request.form['parent_id']))
            else:
                return redirect(url_for('team_detail', team_id=team_id))
        
        user_id = session['user_id']
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        parent_id = request.form.get('parent_id', type=int)

        if 'edit_id' in request.form:
            # Update existing comment
            edit_id = request.form['edit_id']
            conn.execute('''
                UPDATE reviews 
                SET comment = ?, timestamp = ? 
                WHERE id = ? AND user_id = ?
            ''', (comment, timestamp, edit_id, user_id))
        else:
            # Insert new comment or reply
            conn.execute('''
                INSERT INTO reviews (user_id, team_id, comment, timestamp, parent_id) 
                VALUES (?, ?, ?, ?, ?)
            ''', (user_id, team_id, comment, timestamp, parent_id))
        
        conn.commit()
        return redirect(url_for('team_detail', team_id=team_id))

    # Get all comments and replies
    reviews = conn.execute('''
        SELECT r.*, u.name, r.id, r.user_id, r.parent_id
        FROM reviews r
        JOIN users u ON r.user_id = u.id
        WHERE r.team_id = ?
        ORDER BY r.timestamp DESC
    ''', (team_id,)).fetchall()

    conn.close()
    return render_template(
        'team_detail.html',
        team=team,
        batting=batting,
        pitching=pitching,
        fielding=fielding,
        reviews=reviews,
        edit_comment=edit_comment,
        reply_to=reply_to,
        reply_author=reply_author
    )

@app.route('/delete_comment/<int:comment_id>', methods=['POST'])
def delete_comment(comment_id):
    if 'user_id' not in session:
        flash("You must be logged in to delete comments.", "error")
        return redirect(url_for('login'))

    conn = get_db_connection()
    comment = conn.execute('SELECT * FROM reviews WHERE id = ?', (comment_id,)).fetchone()
    
    if not comment or comment['user_id'] != session['user_id']:
        flash("You can only delete your own comments.", "error")
        conn.close()
        return redirect(url_for('team_detail', team_id=comment['team_id']))

    # Delete the comment and its replies
    conn.execute('DELETE FROM reviews WHERE id = ? OR parent_id = ?', (comment_id, comment_id))
    conn.commit()
    team_id = comment['team_id']
    conn.close()

    flash("Comment deleted successfully.", "success")
    return redirect(url_for('team_detail', team_id=team_id))

@app.route('/profile', methods=['GET', 'POST'])
def profile():
    if 'user_id' not in session:
        flash("Please log in to view your profile.", "error")
        return redirect(url_for('login'))

    user_id = session['user_id']
    conn = get_db_connection()

    if request.method == 'POST':
        name = request.form['name']
        email = request.form['email']
        image_file = request.files.get('image')
        favorite_team = request.form.get('favorite_team')
        
        # Form validation
        errors = []
        
        # Validate email format
        if not email:
            errors.append("Email is required")
        elif not validate_email(email):
            errors.append("Invalid email format. Please use a valid email address.")
        
        # Check if the email is already used by a different user
        if email:
            email_check = conn.execute('SELECT id FROM users WHERE email = ? AND id != ?', 
                                     (email, user_id)).fetchone()
            if email_check:
                errors.append("This email is already registered to another user.")
        
        # If there are validation errors, show them and return to the form
        if errors:
            for error in errors:
                flash(error, 'error')
            user = conn.execute('SELECT * FROM users WHERE id = ?', (user_id,)).fetchone()
            conn.close()
            return render_template('profile.html', user=user)
        
        # Continue with profile update
        image_filename = None
        if image_file and allowed_file(image_file.filename):
            image_filename = secure_filename(image_file.filename)
            image_file.save(os.path.join(app.config['UPLOAD_FOLDER'], image_filename))
        else:
            # Keep existing image if no new one uploaded
                current = conn.execute('SELECT image FROM users WHERE id = ?', (user_id,)).fetchone()
                image_filename = current['image'] if current and current['image'] else None

        conn.execute('UPDATE users SET name = ?, email = ?, image = ?, favorite_team = ? WHERE id = ?',
                     (name, email, image_filename, favorite_team, user_id))
        conn.commit()
        session['user_name'] = name

        flash("✅ Profile updated!", "success")

    user = conn.execute('SELECT * FROM users WHERE id = ?', (user_id,)).fetchone()
    conn.close()
    print("FAVORITE TEAM:", user['favorite_team'])

    return render_template('profile.html', user=user)

# Read GLM coefficients from CSV files
def read_glm_coefficients(filepath):
    coeffs_df = pd.read_csv(filepath)
    return {row['variable']: row['coefficient'] for _, row in coeffs_df.iterrows()}

# Load coefficients from CSV files
glm_home_coeffs = read_glm_coefficients('data/glm_home_coefficients.csv')
glm_away_coeffs = read_glm_coefficients('data/glm_away_coefficients.csv')

# Rename intercept key if needed
if '(Intercept)' in glm_home_coeffs:
    glm_home_coeffs['intercept'] = glm_home_coeffs.pop('(Intercept)')
if '(Intercept)' in glm_away_coeffs:
    glm_away_coeffs['intercept'] = glm_away_coeffs.pop('(Intercept)')

def predict_poisson(stats, coeffs):
    result = coeffs['intercept']
    for key in coeffs:
        if key != 'intercept':
            result += coeffs[key] * stats.get(key, 0)
    return math.exp(result)

@app.route('/predict', methods=['GET', 'POST'])
def predict():
    conn = sqlite3.connect('database.db')
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()
    teams = cur.execute("SELECT team_name FROM team_elo ORDER BY team_name").fetchall()

    result = None

    if request.method == 'POST':
        home = request.form['home_team']
        away = request.form['away_team']
        
        # Normalize names between tables
        team_name_lookup = {
            "Arizona Diamondbacks": "Diamondbacks",
            "Atlanta Braves": "Braves",
            "Baltimore Orioles": "Orioles",
            "Boston Red Sox": "Red Sox",
            "Chicago Cubs": "Cubs",
            "Chicago White Sox": "White Sox",
            "Cincinnati Reds": "Reds",
            "Cleveland Guardians": "Guardians",
            "Colorado Rockies": "Rockies",
            "Detroit Tigers": "Tigers",
            "Houston Astros": "Astros",
            "Kansas City Royals": "Royals",
            "Los Angeles Angels": "Angels",
            "Los Angeles Dodgers": "Dodgers",
            "Miami Marlins": "Marlins",
            "Milwaukee Brewers": "Brewers",
            "Minnesota Twins": "Twins",
            "New York Mets": "Mets",
            "New York Yankees": "Yankees",
            "Oakland Athletics": "Athletics",
            "Philadelphia Phillies": "Phillies",
            "Pittsburgh Pirates": "Pirates",
            "San Diego Padres": "Padres",
            "San Francisco Giants": "Giants",
            "Seattle Mariners": "Mariners",
            "St. Louis Cardinals": "Cardinals",
            "Tampa Bay Rays": "Rays",
            "Texas Rangers": "Rangers",
            "Toronto Blue Jays": "Blue Jays",
            "Washington Nationals": "Nationals"
        }

        # Get stats for both teams
        def get_team_stats(team):
            stats = {}
            short_name = team_name_lookup.get(team, team)  # fallback = same name

            # Get batting, pitching, and fielding
            for table in ['team_batting', 'team_pitching', 'team_fielding']:
                row = cur.execute(f"SELECT * FROM {table} WHERE team_name = ?", (short_name,)).fetchone()
                if row:
                    stats.update(dict(row))

            # Get Elo
            elo_row = cur.execute("SELECT final_elo FROM team_elo WHERE team_name = ?", (team,)).fetchone()
            if elo_row:
                stats['elo_before'] = elo_row['final_elo']  # renamed to match R model

            return stats

        home_stats = get_team_stats(home)
        away_stats = get_team_stats(away)
        
        print("✅ HOME STATS:", home_stats)
        print("✅ AWAY STATS:", away_stats)

        if not home_stats or not away_stats:
            print("❌ Could not retrieve one or both team stat sets. Check team name or DB entries.")
            return render_template('predict.html', teams=teams, result={"error": "Missing stats for one or both teams."})

        # Combine for model input
        model_input_home = {
            **{f'home_{k}': v for k, v in home_stats.items() if k != 'team_name'},
            **{f'away_{k}': v for k, v in away_stats.items() if k != 'team_name'}
        }

        model_input_away = {
            **{f'away_{k}': v for k, v in away_stats.items() if k != 'team_name'},
            **{f'home_{k}': v for k, v in home_stats.items() if k != 'team_name'}
        }

        predicted_home_runs = predict_poisson(model_input_home, glm_home_coeffs)

        print("AWAY MODEL INPUT:", model_input_away)
        predicted_away_runs = predict_poisson(model_input_away, glm_away_coeffs)
        predicted_diff = predicted_home_runs - predicted_away_runs

        # Logistic regression
        intercept = 0.5111408
        diff_coeff = 0.5201442
        logit = intercept + diff_coeff * predicted_diff
        home_win_prob = 1 / (1 + math.exp(-logit))
        away_win_prob = 1 - home_win_prob

        result = {
            'home_team': home,
            'away_team': away,
            'predicted_home_runs': round(predicted_home_runs, 2),
            'predicted_away_runs': round(predicted_away_runs, 2),
            'home_win_prob': round(home_win_prob * 100, 1),
            'away_win_prob': round(away_win_prob * 100, 1)
        }

    conn.close()
    return render_template('predict.html', teams=teams, result=result)

if __name__ == '__main__':
    app.run(debug=True)
