import pygame
import math
import random
import json

# Initialize Pygame
pygame.init()
WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Asteroids")
clock = pygame.time.Clock()

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
RED = (255, 0, 0)
ORANGE = (255, 165, 0)
GREEN = (0, 255, 0)
BLUE = (0, 0, 255)

# Load sounds (comment out if files are missing)
try:
    pygame.mixer.init()
    shoot_sound = pygame.mixer.Sound("shoot.wav")
    explosion_sound = pygame.mixer.Sound("explosion.wav")
    ufo_hum_small = pygame.mixer.Sound("ufo_hum_small.wav")
    ufo_hum_large = pygame.mixer.Sound("ufo_hum_large.wav")
except:
    print("Sound files missing; running without audio")
    shoot_sound = explosion_sound = ufo_hum_small = ufo_hum_large = None

# Ship
ship = {
    "x": WIDTH / 2,
    "y": HEIGHT / 2,
    "angle": 0,
    "dx": 0,
    "dy": 0,
    "radius": 15,
    "thrusting": False,
    "mass": 10
}

# Game objects
bullets = []
asteroids = []
enemy_bullets = []
particles = []
ufo = None
dark_matter_clouds = []
black_holes = []
keys = set()

# Game settings
SHIP_SPEED = 0.2
ROTATION_SPEED = 5
BULLET_SPEED = 7
ASTEROID_SPEED = 2
ASTEROID_SIZES = [40, 20, 10]
FRICTION = 0.99
UFO_BULLET_SPEED = 5
UFO_SHOOT_INTERVAL = 120
GRAVITY_CONSTANT = 0.6
MIN_DISTANCE = 10
BREAKUP_SPEED = 1
SPEED_OF_LIGHT = 20  # Game-defined speed of light (pixels/frame)

# Game modes
GAME_MODES = {
    "classic": {
        "name": "Classic Mode",
        "initial_asteroids": 4,
        "asteroids_per_wave": lambda level: 4 + level - 1,
        "ufo_spawn_min": 600,
        "ufo_spawn_max": 1200,
        "lives": 3,
        "score_multiplier": 1,
        "shot_limit": 3,
        "shot_cooldown": 30,
        "time_limit": None,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": False
    },
    "survival": {
        "name": "Survival Mode",
        "initial_asteroids": 6,
        "asteroids_per_wave": lambda level: 6,
        "ufo_spawn_min": 300,
        "ufo_spawn_max": 600,
        "lives": 1,
        "score_multiplier": 2,
        "shot_limit": None,
        "shot_cooldown": 0,
        "time_limit": None,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": False
    },
    "time_attack": {
        "name": "Time Attack Mode",
        "initial_asteroids": 8,
        "asteroids_per_wave": lambda level: 8,
        "ufo_spawn_min": 300,
        "ufo_spawn_max": 600,
        "lives": float('inf'),
        "score_multiplier": 1,
        "shot_limit": 5,
        "shot_cooldown": 18,
        "time_limit": 3600,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": False
    },
    "newtonian_gravity": {
        "name": "Newtonian Gravity Mode",
        "initial_asteroids": 5,
        "asteroids_per_wave": lambda level: 5 + level - 1,
        "ufo_spawn_min": 600,
        "ufo_spawn_max": 1200,
        "lives": 3,
        "score_multiplier": 1,
        "shot_limit": 3,
        "shot_cooldown": 30,
        "time_limit": None,
        "newtonian_gravity": True,
        "dark_matter": False,
        "relativistic": False
    },
    "dark_matter": {
        "name": "Dark Matter Mode",
        "initial_asteroids": 6,
        "asteroids_per_wave": lambda level: 6 + level - 1,
        "ufo_spawn_min": 720,
        "ufo_spawn_max": 1080,
        "lives": 2,
        "score_multiplier": 1.5,
        "shot_limit": 3,
        "shot_cooldown": 30,
        "time_limit": None,
        "newtonian_gravity": True,
        "dark_matter": True,
        "relativistic": False
    },
    "relativistic": {
        "name": "Relativistic Mode",
        "initial_asteroids": 5,
        "asteroids_per_wave": lambda level: 5 + level - 1,
        "ufo_spawn_min": 600,
        "ufo_spawn_max": 1200,
        "lives": 3,
        "score_multiplier": 1.5,
        "shot_limit": 3,
        "shot_cooldown": 30,
        "time_limit": None,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": True
    }
}
current_mode = "classic"

# Game state
score = 0
lives = GAME_MODES[current_mode]["lives"]
level = 1
high_score = 0
ufo_spawn_timer = random.randint(GAME_MODES[current_mode]["ufo_spawn_min"], GAME_MODES[current_mode]["ufo_spawn_max"])
game_state = "menu"
shot_count = 0
shoot_cooldown = 0
shot_reset_timer = 60
timer = GAME_MODES[current_mode]["time_limit"]
font = pygame.font.SysFont("arial", 24)

# Load high score
try:
    with open("highscore.json", "r") as f:
        high_score = json.load(f).get("high_score", 0)
except:
    high_score = 0

def save_high_score():
    global high_score
    if score > high_score:
        high_score = score
        with open("highscore.json", "w") as f:
            json.dump({"high_score": high_score}, f)

# Reset game
def reset_game():
    global ship, asteroids, bullets, enemy_bullets, particles, ufo, score, lives, level, ufo_spawn_timer, shot_count, shoot_cooldown, shot_reset_timer, timer, black_holes
    ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
    ship["dx"], ship["dy"] = 0, 0
    ship["angle"] = 0
    asteroids.clear()
    bullets.clear()
    enemy_bullets.clear()
    particles.clear()
    ufo = None
    black_holes.clear()
    score = 0
    lives = GAME_MODES[current_mode]["lives"]
    level = 1
    ufo_spawn_timer = random.randint(GAME_MODES[current_mode]["ufo_spawn_min"], GAME_MODES[current_mode]["ufo_spawn_max"])
    shot_count = 0
    shoot_cooldown = 0
    shot_reset_timer = 60
    timer = GAME_MODES[current_mode]["time_limit"]
    for _ in range(GAME_MODES[current_mode]["initial_asteroids"]):
        spawn_asteroid(ASTEROID_SIZES[0])
    spawn_dark_matter_clouds()
    if GAME_MODES[current_mode].get("relativistic", False):
        spawn_black_holes()

# Spawn asteroids
def spawn_asteroid(size, x=None, y=None):
    num_vertices = random.randint(6, 12)
    asteroid = {
        "x": x or random.randint(0, WIDTH),
        "y": y or random.randint(0, HEIGHT),
        "dx": (random.random() - 0.5) * ASTEROID_SPEED * (1 + (level - 1) * 0.1),
        "dy": (random.random() - 0.5) * ASTEROID_SPEED * (1 + (level - 1) * 0.1),
        "radius": size,
        "vertices": num_vertices,
        "offsets": [random.uniform(0.8, 1.2) for _ in range(num_vertices)],
        "mass": 20 if size == 40 else 10 if size == 20 else 5
    }
    if math.hypot(asteroid["x"] - ship["x"], asteroid["y"] - ship["y"]) < asteroid["radius"] + ship["radius"] + 50:
        asteroid["x"] = random.randint(0, WIDTH)
        asteroid["y"] = random.randint(0, HEIGHT)
    for bh in black_holes:
        if math.hypot(asteroid["x"] - bh["x"], asteroid["y"] - bh["y"]) < bh["accretion_radius"] + asteroid["radius"]:
            asteroid["x"] = random.randint(0, WIDTH)
            asteroid["y"] = random.randint(0, HEIGHT)
    asteroids.append(asteroid)

# Spawn UFO
def spawn_ufo():
    global ufo
    ufo_type = random.choice(["small", "large"])
    radius = 15 if ufo_type == "small" else 25
    speed = 3 if ufo_type == "small" else 1.5
    points = 2000 if ufo_type == "small" else 1000
    shoot_interval = 60 if ufo_type == "small" else 120
    side = random.choice([-1, 1])
    ufo = {
        "x": 0 if side == 1 else WIDTH,
        "y": random.randint(100, HEIGHT - 100),
        "dx": side * speed,
        "dy": 0,
        "radius": radius,
        "shoot_timer": shoot_interval,
        "type": ufo_type,
        "points": points,
        "change_direction_timer": random.randint(30, 90),
        "mass": 8 if ufo_type == "small" else 15
    }
    for bh in black_holes:
        if math.hypot(ufo["x"] - bh["x"], ufo["y"] - bh["y"]) < bh["accretion_radius"] + ufo["radius"]:
            ufo["x"] = 0 if side == 1 else WIDTH
            ufo["y"] = random.randint(100, HEIGHT - 100)
    if ufo["type"] == "small" and ufo_hum_small:
        ufo_hum_small.play(-1)
    elif ufo_hum_large:
        ufo_hum_large.play(-1)

# Spawn dark matter clouds
def spawn_dark_matter_clouds():
    global dark_matter_clouds
    dark_matter_clouds.clear()
    if not GAME_MODES[current_mode].get("dark_matter", False):
        return
    for _ in range(random.randint(3, 5)):
        cloud = {
            "x": random.randint(100, WIDTH - 100),
            "y": random.randint(100, HEIGHT - 100),
            "mass": 50,
            "radius": 100,
            "dx": (random.random() - 0.5) * 3,
            "dy": (random.random() - 0.5) * 3
        }
        if math.hypot(cloud["x"] - ship["x"], cloud["y"] - ship["y"]) < cloud["radius"] + ship["radius"] + 50:
            cloud["x"] = random.randint(100, WIDTH - 100)
            cloud["y"] = random.randint(100, HEIGHT - 100)
        for bh in black_holes:
            if math.hypot(cloud["x"] - bh["x"], cloud["y"] - bh["y"]) < bh["accretion_radius"] + cloud["radius"]:
                cloud["x"] = random.randint(100, WIDTH - 100)
                cloud["y"] = random.randint(100, HEIGHT - 100)
        dark_matter_clouds.append(cloud)

# Spawn black holes
def spawn_black_holes():
    global black_holes
    black_holes.clear()
    num_black_holes = random.randint(1, 2)
    for _ in range(num_black_holes):
        bh = {
            "x": random.randint(100, WIDTH - 100),
            "y": random.randint(100, HEIGHT - 100),
            "mass": 100,
            "radius": 10,  # Event horizon
            "accretion_radius": 50,  # Accretion disk
            "dx": (random.random() - 0.5) * 0.5,
            "dy": (random.random() - 0.5) * 0.5
        }
        if math.hypot(bh["x"] - ship["x"], bh["y"] - ship["y"]) < bh["accretion_radius"] + ship["radius"] + 100:
            bh["x"] = random.randint(100, WIDTH - 100)
            bh["y"] = random.randint(100, HEIGHT - 100)
        black_holes.append(bh)

# Apply gravity
def apply_gravity():
    if not (GAME_MODES[current_mode]["newtonian_gravity"] or GAME_MODES[current_mode].get("dark_matter", False) or GAME_MODES[current_mode].get("relativistic", False)):
        return
    massive_objects = [ship] + asteroids + ([ufo] if ufo is not None else []) + (dark_matter_clouds if GAME_MODES[current_mode].get("dark_matter", False) else []) + black_holes
    accelerations = [{"ax": 0, "ay": 0} for _ in massive_objects]
    for i, obj1 in enumerate(massive_objects):
        for j, obj2 in enumerate(massive_objects):
            if i == j:
                continue
            dx = obj2["x"] - obj1["x"]
            dy = obj2["y"] - obj1["y"]
            if dx > WIDTH / 2:
                dx -= WIDTH
            elif dx < -WIDTH / 2:
                dx += WIDTH
            if dy > HEIGHT / 2:
                dy -= HEIGHT
            elif dy < -HEIGHT / 2:
                dy += HEIGHT
            r = math.hypot(dx, dy)
            if r < MIN_DISTANCE:
                r = MIN_DISTANCE
            if r > 0:
                force = GRAVITY_CONSTANT * obj1["mass"] * obj2["mass"] / (r * r)
                if "radius" in obj2 and obj2 in black_holes:
                    force *= 5  # Stronger gravity for black holes
                accelerations[i]["ax"] += force * dx / (r * obj1["mass"])
                accelerations[i]["ay"] += force * dy / (r * obj1["mass"])
    for obj, acc in zip(massive_objects, accelerations):
        obj["dx"] += acc["ax"]
        obj["dy"] += acc["ay"]

# Apply relativistic effects
def apply_relativistic_effects(obj, update=True):
    if not GAME_MODES[current_mode].get("relativistic", False):
        return True
    speed = math.hypot(obj["dx"], obj["dy"])
    if speed > 0.5 * SPEED_OF_LIGHT:
        scale = 0.5 * SPEED_OF_LIGHT / speed
        obj["dx"] *= scale
        obj["dy"] *= scale
        speed = 0.5 * SPEED_OF_LIGHT
    gamma = 1 / math.sqrt(1 - (speed**2 / SPEED_OF_LIGHT**2)) if speed < SPEED_OF_LIGHT else 10
    if update and random.random() > 1 / gamma:
        return False
    obj["contraction"] = max(0.8, 1 / gamma) if obj is ship else 1 / gamma
    obj["motion_angle"] = math.atan2(obj["dy"], obj["dx"]) if speed > 0 else 0
    # Doppler shift and headlight effect
    rel_vx = obj["dx"] - ship["dx"]
    rel_vy = obj["dy"] - ship["dy"]
    rel_speed = math.hypot(rel_vx, rel_vy)
    if rel_speed > 0:
        cos_theta = (rel_vx * (ship["x"] - obj["x"]) + rel_vy * (ship["y"] - obj["y"])) / (rel_speed * math.hypot(ship["x"] - obj["x"], ship["y"] - obj["y"]))
        doppler_factor = rel_speed / SPEED_OF_LIGHT * cos_theta  # Simplified
        obj["doppler"] = doppler_factor
        obj["brightness"] = min(255, 255 * (1 + gamma / 2)) if cos_theta > 0 else 255
    else:
        obj["doppler"] = 0
        obj["brightness"] = 255
    return True

# Initialize asteroids
for _ in range(GAME_MODES[current_mode]["initial_asteroids"]):
    spawn_asteroid(ASTEROID_SIZES[0])

# Game loop
running = True
space_pressed = False
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            if game_state == "menu":
                if event.key == pygame.K_1:
                    current_mode = "classic"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_2:
                    current_mode = "survival"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_3:
                    current_mode = "time_attack"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_4:
                    current_mode = "newtonian_gravity"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_5:
                    current_mode = "dark_matter"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_6:
                    current_mode = "relativistic"
                    game_state = "playing"
                    reset_game()
            elif game_state == "game_over" and event.key == pygame.K_r:
                game_state = "playing"
                reset_game()
            elif game_state == "game_over" and event.key == pygame.K_q:
                running = False
            keys.add(event.key)
        elif event.type == pygame.KEYUP:
            keys.discard(event.key)

    if game_state == "menu":
        screen.fill(BLACK)
        title_text = font.render("Asteroids", True, WHITE)
        start_text = font.render("Press 1: Classic, 2: Survival, 3: Time Attack, 4: Newtonian Gravity, 5: Dark Matter, 6: Relativistic", True, WHITE)
        instructions = font.render("Left/Right: Rotate, Up: Thrust, Space: Shoot", True, WHITE)
        screen.blit(title_text, (WIDTH // 2 - 50, HEIGHT // 2 - 50))
        screen.blit(start_text, (WIDTH // 2 - 200, HEIGHT // 2))
        screen.blit(instructions, (WIDTH // 2 - 150, HEIGHT // 2 + 50))
        pygame.display.flip()
        continue

    if game_state == "game_over":
        save_high_score()
        screen.fill(BLACK)
        game_over_text = font.render(f"Game Over! Score: {score} High: {high_score}", True, WHITE)
        restart_text = font.render("Press R to Restart, Q to Quit", True, WHITE)
        screen.blit(game_over_text, (WIDTH // 2 - 150, HEIGHT // 2))
        screen.blit(restart_text, (WIDTH // 2 - 120, HEIGHT // 2 + 50))
        pygame.display.flip()
        continue

    # Apply gravity
    apply_gravity()

    # Update ship
    if apply_relativistic_effects(ship):
        if pygame.K_LEFT in keys:
            ship["angle"] += ROTATION_SPEED
        if pygame.K_RIGHT in keys:
            ship["angle"] -= ROTATION_SPEED
        ship["thrusting"] = pygame.K_UP in keys
        if ship["thrusting"]:
            ship["dx"] += math.cos(math.radians(ship["angle"])) * SHIP_SPEED
            ship["dy"] -= math.sin(math.radians(ship["angle"])) * SHIP_SPEED
            for _ in range(2):
                angle = math.radians(ship["angle"] + 180 + random.uniform(-20, 20))
                particles.append({
                    "x": ship["x"] + math.cos(math.radians(ship["angle"] + 180)) * ship["radius"],
                    "y": ship["y"] - math.sin(math.radians(ship["angle"] + 180)) * ship["radius"],
                    "dx": math.cos(angle) * 3 + ship["dx"],
                    "dy": -math.sin(angle) * 3 + ship["dy"],
                    "life": 15
                })
    ship["x"] += ship["dx"]
    ship["y"] += ship["dy"]
    ship["dx"] *= FRICTION
    ship["dy"] *= FRICTION

    # Wrap ship
    ship["x"] %= WIDTH
    ship["y"] %= HEIGHT

    # Check black hole collision
    for bh in black_holes[:]:
        if math.hypot(ship["x"] - bh["x"], ship["y"] - bh["y"]) < bh["radius"] + ship["radius"]:
            lives -= 1
            ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
            ship["dx"], ship["dy"] = 0, 0
            if lives <= 0 and current_mode != "time_attack":
                game_state = "game_over"

    # Shoot
    if pygame.K_SPACE in keys and not space_pressed and shoot_cooldown <= 0:
        if GAME_MODES[current_mode]["shot_limit"] is None or shot_count < GAME_MODES[current_mode]["shot_limit"]:
            # Apply aberration to bullet direction
            bullet_angle = math.radians(ship["angle"])
            ship_speed = math.hypot(ship["dx"], ship["dy"])
            if ship_speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
                ship_vx = ship["dx"] / ship_speed
                ship_vy = ship["dy"] / ship_speed
                aberration = ship_speed / SPEED_OF_LIGHT
                cos_theta = math.cos(bullet_angle) * ship_vx + math.sin(bullet_angle) * ship_vy
                bullet_angle += math.asin(aberration * cos_theta)
            bullets.append({
                "x": ship["x"] + math.cos(bullet_angle) * ship["radius"],
                "y": ship["y"] - math.sin(bullet_angle) * ship["radius"],
                "dx": math.cos(bullet_angle) * BULLET_SPEED + ship["dx"],
                "dy": -math.sin(bullet_angle) * BULLET_SPEED + ship["dy"],
                "life": 60
            })
            if shoot_sound:
                shoot_sound.play()
            shot_count += 1
            shot_reset_timer = 60
            if GAME_MODES[current_mode]["shot_limit"] and shot_count >= GAME_MODES[current_mode]["shot_limit"]:
                shoot_cooldown = GAME_MODES[current_mode]["shot_cooldown"]
            space_pressed = True
    if pygame.K_SPACE not in keys:
        space_pressed = False

    # Update bullets
    for bullet in bullets[:]:
        bullet["x"] += bullet["dx"]
        bullet["y"] += bullet["dy"]
        bullet["life"] -= 1
        if bullet["x"] < 0 or bullet["x"] > WIDTH or bullet["y"] < 0 or bullet["y"] > HEIGHT or bullet["life"] <= 0:
            bullets.remove(bullet)

    # Update enemy bullets
    for bullet in enemy_bullets[:]:
        bullet["x"] += bullet["dx"]
        bullet["y"] += bullet["dy"]
        bullet["life"] -= 1
        if bullet["x"] < 0 or bullet["x"] > WIDTH or bullet["y"] < 0 or bullet["y"] > HEIGHT or bullet["life"] <= 0:
            enemy_bullets.remove(bullet)
        elif math.hypot(bullet["x"] - ship["x"], bullet["y"] - ship["y"]) < ship["radius"]:
            enemy_bullets.remove(bullet)
            lives -= 1
            ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
            ship["dx"], ship["dy"] = 0, 0
            if lives <= 0 and current_mode != "time_attack":
                game_state = "game_over"

    # Update asteroids
    for asteroid in asteroids[:]:
        if apply_relativistic_effects(asteroid):
            asteroid["x"] += asteroid["dx"]
            asteroid["y"] += asteroid["dy"]
            asteroid["x"] %= WIDTH
            asteroid["y"] %= HEIGHT
            for bullet in bullets[:]:
                if math.hypot(bullet["x"] - asteroid["x"], bullet["y"] - asteroid["y"]) < asteroid["radius"]:
                    bullets.remove(bullet)
                    score += 100 * (ASTEROID_SIZES.index(asteroid["radius"]) + 1) * GAME_MODES[current_mode]["score_multiplier"]
                    if explosion_sound:
                        explosion_sound.play()
                    for _ in range(10):
                        particles.append({
                            "x": asteroid["x"],
                            "y": asteroid["y"],
                            "dx": (random.random() - 0.5) * 5,
                            "dy": (random.random() - 0.5) * 5,
                            "life": 30
                        })
                    if asteroid["radius"] > ASTEROID_SIZES[2]:
                        new_size = ASTEROID_SIZES[ASTEROID_SIZES.index(asteroid["radius"]) + 1]
                        angle = random.random() * 2 * math.pi
                        dx1 = math.cos(angle) * BREAKUP_SPEED
                        dy1 = math.sin(angle) * BREAKUP_SPEED
                        dx2 = -dx1
                        dy2 = -dy1
                        spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                        asteroids[-1]["dx"] += dx1
                        asteroids[-1]["dy"] += dy1
                        spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                        asteroids[-1]["dx"] += dx2
                        asteroids[-1]["dy"] += dy2
                    asteroids.remove(asteroid)
                    break
            if math.hypot(ship["x"] - asteroid["x"], ship["y"] - asteroid["y"]) < ship["radius"] + asteroid["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                if lives <= 0 and current_mode != "time_attack":
                    game_state = "game_over"
        else:
            asteroid["x"] %= WIDTH
            asteroid["y"] %= HEIGHT
        for bh in black_holes:
            if math.hypot(asteroid["x"] - bh["x"], asteroid["y"] - bh["y"]) < bh["radius"] + asteroid["radius"]:
                asteroids.remove(asteroid)
                break

    # Update UFO
    if ufo is not None:
        if apply_relativistic_effects(ufo):
            ufo["x"] += ufo["dx"]
            ufo["y"] += ufo["dy"]
            ufo["change_direction_timer"] -= 1
            if ufo["change_direction_timer"] <= 0:
                ufo["dy"] = (random.random() - 0.5) * 2
                ufo["change_direction_timer"] = random.randint(30, 90)
            ufo["shoot_timer"] -= 1
            if ufo["shoot_timer"] <= 0:
                angle = math.atan2(ship["y"] - ufo["y"], ship["x"] - ufo["x"])
                if ufo["type"] == "small":
                    angle += random.uniform(-0.2, 0.2)
                enemy_bullets.append({
                    "x": ufo["x"],
                    "y": ufo["y"],
                    "dx": math.cos(angle) * UFO_BULLET_SPEED,
                    "dy": math.sin(angle) * UFO_BULLET_SPEED,
                    "life": 60
                })
                ufo["shoot_timer"] = UFO_SHOOT_INTERVAL
            for bullet in bullets[:]:
                if math.hypot(bullet["x"] - ufo["x"], bullet["y"] - ufo["y"]) < ufo["radius"]:
                    bullets.remove(bullet)
                    score += ufo["points"] * GAME_MODES[current_mode]["score_multiplier"]
                    if explosion_sound:
                        explosion_sound.play()
                    for _ in range(15):
                        particles.append({
                            "x": ufo["x"],
                            "y": ufo["y"],
                            "dx": (random.random() - 0.5) * 5,
                            "dy": (random.random() - 0.5) * 5,
                            "life": 30
                        })
                    ufo = None
                    if ufo_hum_small:
                        ufo_hum_small.stop()
                    if ufo_hum_large:
                        ufo_hum_large.stop()
                    break
            if ufo is not None and math.hypot(ship["x"] - ufo["x"], ship["y"] - ufo["y"]) < ship["radius"] + ufo["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                ufo = None
                if ufo_hum_small:
                    ufo_hum_small.stop()
                if ufo_hum_large:
                    ufo_hum_large.stop()
                if lives <= 0 and current_mode != "time_attack":
                    game_state = "game_over"
        if ufo is not None:
            for bh in black_holes:
                if math.hypot(ufo["x"] - bh["x"], ufo["y"] - bh["y"]) < bh["radius"] + ufo["radius"]:
                    ufo = None
                    if ufo_hum_small:
                        ufo_hum_small.stop()
                    if ufo_hum_large:
                        ufo_hum_large.stop()
                    break
        if ufo is not None and (ufo["x"] < -ufo["radius"] or ufo["x"] > WIDTH + ufo["radius"]):
            ufo = None
            if ufo_hum_small:
                ufo_hum_small.stop()
            if ufo_hum_large:
                ufo_hum_large.stop()

    # Spawn UFO
    ufo_spawn_timer -= 1
    if ufo_spawn_timer <= 0 and ufo is None:
        spawn_ufo()
        ufo_spawn_timer = random.randint(GAME_MODES[current_mode]["ufo_spawn_min"], GAME_MODES[current_mode]["ufo_spawn_max"])

    # Update dark matter clouds
    for cloud in dark_matter_clouds:
        cloud["x"] += cloud["dx"]
        cloud["y"] += cloud["dy"]
        cloud["x"] %= WIDTH
        cloud["y"] %= HEIGHT

    # Update black holes
    for bh in black_holes:
        bh["x"] += bh["dx"]
        bh["y"] += bh["dy"]
        bh["x"] %= WIDTH
        bh["y"] %= HEIGHT

    # Check for wave progression
    if len(asteroids) == 0:
        level += 1
        asteroid_count = GAME_MODES[current_mode]["asteroids_per_wave"](level)
        for _ in range(asteroid_count):
            spawn_asteroid(ASTEROID_SIZES[0])
        if GAME_MODES[current_mode].get("relativistic", False):
            spawn_black_holes()

    # Update timers
    if shoot_cooldown > 0:
        shoot_cooldown -= 1
    if shot_reset_timer > 0:
        shot_reset_timer -= 1
    else:
        shot_count = 0
    if timer is not None:
        timer -= 1
        if timer <= 0:
            game_state = "game_over"

    # Update particles
    for particle in particles[:]:
        particle["x"] += particle["dx"]
        particle["y"] += particle["dy"]
        particle["life"] -= 1
        if particle["life"] <= 0:
            particles.remove(particle)

    # Draw
    screen.fill(BLACK)
    # Draw black holes
    if GAME_MODES[current_mode].get("relativistic", False):
        for bh in black_holes:
            # Accretion disk
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 165, 0, 50), (int(bh["x"]), int(bh["y"])), bh["accretion_radius"])
            screen.blit(surface, (0, 0))
            # Event horizon
            pygame.draw.circle(screen, BLACK, (int(bh["x"]), int(bh["y"])), bh["radius"])

    # Draw ship
    apply_relativistic_effects(ship, update=False)  # Update visuals without time dilation
    contraction = ship.get("contraction", 1)
    motion_angle = ship.get("motion_angle", 0)
    doppler = ship.get("doppler", 0)
    brightness = ship.get("brightness", 255)
    color = (
        min(255, int(255 * (1 - doppler))),
        min(255, int(255 * (1 - abs(doppler)))),
        min(255, int(255 * (1 + doppler)))
    ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
    ship_points = [
        (
            ship["x"] + (math.cos(math.radians(ship["angle"])) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"])) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"])) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"])) * ship["radius"] * math.sin(motion_angle)**2)
        ),
        (
            ship["x"] + (math.cos(math.radians(ship["angle"] + 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"] + 140)) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"] + 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"] + 140)) * ship["radius"] * math.sin(motion_angle)**2)
        ),
        (
            ship["x"] + (math.cos(math.radians(ship["angle"] - 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"] - 140)) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"] - 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"] - 140)) * ship["radius"] * math.sin(motion_angle)**2)
        )
    ]
    pygame.draw.polygon(screen, color, ship_points, 1)
    # Draw nose indicator
    nose_x = ship["x"] + math.cos(math.radians(ship["angle"])) * ship["radius"]
    nose_y = ship["y"] - math.sin(math.radians(ship["angle"])) * ship["radius"]
    surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
    pygame.draw.circle(surface, (255, 0, 0, 100), (int(nose_x), int(nose_y)), 5)
    pygame.draw.circle(surface, RED, (int(nose_x), int(nose_y)), 3)
    screen.blit(surface, (0, 0))

    for bullet in bullets:
        pygame.draw.circle(screen, RED, (int(bullet["x"]), int(bullet["y"])), 2)

    for bullet in enemy_bullets:
        pygame.draw.circle(screen, GREEN, (int(bullet["x"]), int(bullet["y"])), 2)

    # Draw asteroids
    for asteroid in asteroids:
        apply_relativistic_effects(asteroid, update=False)  # Update visuals
        contraction = asteroid.get("contraction", 1)
        motion_angle = asteroid.get("motion_angle", 0)
        doppler = asteroid.get("doppler", 0)
        brightness = asteroid.get("brightness", 255)
        # Aberration
        ship_speed = math.hypot(ship["dx"], ship["dy"])
        dx = asteroid["x"] - ship["x"]
        dy = asteroid["y"] - ship["y"]
        if dx > WIDTH / 2:
            dx -= WIDTH
        elif dx < -WIDTH / 2:
            dx += WIDTH
        if dy > HEIGHT / 2:
            dy -= HEIGHT
        elif dy < -HEIGHT / 2:
            dy += HEIGHT
        r = math.hypot(dx, dy)
        if r > 0 and ship_speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
            theta = math.atan2(dy, dx)
            ship_vx = ship["dx"] / ship_speed if ship_speed > 0 else 0
            ship_vy = ship["dy"] / ship_speed if ship_speed > 0 else 0
            aberration = ship_speed / SPEED_OF_LIGHT
            cos_theta = math.cos(theta) * ship_vx + math.sin(theta) * ship_vy
            theta_prime = theta + math.asin(aberration * cos_theta)
            asteroid_x = ship["x"] + r * math.cos(theta_prime)
            asteroid_y = ship["y"] + r * math.sin(theta_prime)
        else:
            asteroid_x = asteroid["x"]
            asteroid_y = asteroid["y"]
        color = (
            min(255, int(brightness * (1 - doppler))),
            min(255, int(brightness * (1 - abs(doppler)))),
            min(255, int(brightness * (1 + doppler)))
        ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
        points = []
        for i in range(asteroid["vertices"]):
            angle = i * 2 * math.pi / asteroid["vertices"]
            radius = asteroid["radius"] * asteroid["offsets"][i]
            x = asteroid_x + (math.cos(angle) * radius * contraction * math.cos(motion_angle)**2 + math.sin(angle) * radius * math.sin(motion_angle)**2)
            y = asteroid_y + (math.sin(angle) * radius * contraction * math.cos(motion_angle)**2 - math.cos(angle) * radius * math.sin(motion_angle)**2)
            points.append((x, y))
        pygame.draw.polygon(screen, color, points, 1)
        # Gravitational lensing
        if GAME_MODES[current_mode].get("relativistic", False) and math.hypot(asteroid_x - ship["x"], asteroid_y - ship["y"]) < 100 and math.hypot(asteroid["dx"], asteroid["dy"]) > 0.3 * SPEED_OF_LIGHT:
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 255, 255, 20), (int(asteroid_x), int(asteroid_y)), asteroid["radius"] + 5)
            screen.blit(surface, (0, 0))

    # Draw UFO
    if ufo is not None:
        apply_relativistic_effects(ufo, update=False)
        contraction = ufo.get("contraction", 1)
        motion_angle = ufo.get("motion_angle", 0)
        doppler = ufo.get("doppler", 0)
        brightness = ufo.get("brightness", 255)
        # Aberration
        dx = ufo["x"] - ship["x"]
        dy = ufo["y"] - ship["y"]
        if dx > WIDTH / 2:
            dx -= WIDTH
        elif dx < -WIDTH / 2:
            dx += WIDTH
        if dy > HEIGHT / 2:
            dy -= HEIGHT
        elif dy < -HEIGHT / 2:
            dy += HEIGHT
        r = math.hypot(dx, dy)
        if r > 0 and ship_speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
            theta = math.atan2(dy, dx)
            theta_prime = theta + math.asin(aberration * cos_theta)
            ufo_x = ship["x"] + r * math.cos(theta_prime)
            ufo_y = ship["y"] + r * math.sin(theta_prime)
        else:
            ufo_x = ufo["x"]
            ufo_y = ufo["y"]
        color = (
            min(255, int(brightness * (1 - doppler))),
            min(255, int(brightness * (1 - abs(doppler)))),
            min(255, int(brightness * (1 + doppler)))
        ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
        scale = 1.5 if ufo["type"] == "large" else 1
        ufo_points = [
            (
                ufo_x + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                ufo_y + (-ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (-ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                ufo_y + (ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
            )
        ]
        pygame.draw.polygon(screen, color, ufo_points, 1)
        # Gravitational lensing
        if GAME_MODES[current_mode].get("relativistic", False) and math.hypot(ufo_x - ship["x"], ufo_y - ship["y"]) < 100 and math.hypot(ufo["dx"], ufo["dy"]) > 0.3 * SPEED_OF_LIGHT:
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 255, 255, 20), (int(ufo_x), int(ufo_y)), ufo["radius"] + 5)
            screen.blit(surface, (0, 0))

    # Draw relativistic trails
    if GAME_MODES[current_mode].get("relativistic", False):
        for obj in [ship] + asteroids + ([ufo] if ufo is not None else []):
            speed = math.hypot(obj["dx"], obj["dy"])
            if speed > 0.3 * SPEED_OF_LIGHT:
                alpha = int(150 * (speed / (0.5 * SPEED_OF_LIGHT))) if obj is ship else int(100 * (speed / (0.5 * SPEED_OF_LIGHT)))
                trail_length = 10 if obj is ship else 5
                color = (0, 0, 255, min(255, alpha)) if obj is ship else (255, 255, 255, min(255, alpha))
                surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
                pygame.draw.line(surface, color,
                                (obj["x"], obj["y"]),
                                (obj["x"] - obj["dx"] * trail_length, obj["y"] - obj["dy"] * trail_length), 2)
                screen.blit(surface, (0, 0))

    for particle in particles:
        pygame.draw.circle(screen, WHITE, (int(particle["x"]), int(particle["y"])), 2)

    # Draw dark matter clouds
    if GAME_MODES[current_mode].get("dark_matter", False):
        for cloud in dark_matter_clouds:
            visible = any(math.hypot(particle["x"] - cloud["x"], particle["y"] - cloud["y"]) < cloud["radius"] for particle in particles)
            if visible:
                surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
                pygame.draw.circle(surface, (255, 255, 255, 50), (int(cloud["x"]), int(cloud["y"])), cloud["radius"])
                screen.blit(surface, (0, 0))

    score_text = font.render(f"Score: {score}", True, WHITE)
    lives_text = font.render(f"Lives: {int(lives) if lives != float('inf') else '-'}", True, WHITE)
    level_text = font.render(f"Level: {level}", True, WHITE)
    mode_text = font.render(GAME_MODES[current_mode]["name"], True, WHITE)
    timer_text = font.render(f"Time: {int(timer / 60) if timer is not None else '-'}", True, WHITE)
    shots_text = font.render(f"Shots: {GAME_MODES[current_mode]['shot_limit'] - shot_count if GAME_MODES[current_mode]['shot_limit'] else '-'}", True, WHITE)
    screen.blit(score_text, (10, 10))
    screen.blit(lives_text, (10, 40))
    screen.blit(level_text, (10, 70))
    screen.blit(mode_text, (10, 130))
    screen.blit(timer_text, (10, 160))
    screen.blit(shots_text, (10, 190))
    if shoot_cooldown > 0:
        cooldown_text = font.render("Cool-down!", True, RED)
        screen.blit(cooldown_text, (10, 220))

    pygame.display.flip()
    clock.tick(60)

pygame.quit()
