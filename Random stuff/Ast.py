import pygame
import math
import random

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

# Ship
ship = {
    "x": WIDTH / 2,
    "y": HEIGHT / 2,
    "angle": 0,
    "dx": 0,
    "dy": 0,
    "radius": 15,
    "thrusting": False
}

# Game objects
bullets = []
asteroids = []
enemy_bullets = []
ufo = None
keys = set()

# Game settings
SHIP_SPEED = 0.2
ROTATION_SPEED = 5
BULLET_SPEED = 7
ASTEROID_COUNT = 5
ASTEROID_SPEED = 2
ASTEROID_SIZES = [40, 20, 10]
FRICTION = 0.99
UFO_SPAWN_MIN = 600
UFO_SPAWN_MAX = 1200
UFO_SPEED = 2
UFO_SHOOT_INTERVAL = 120
UFO_BULLET_SPEED = 5
UFO_RADIUS = 20

# Game state
score = 0
lives = 3
ufo_spawn_timer = random.randint(UFO_SPAWN_MIN, UFO_SPAWN_MAX)
font = pygame.font.SysFont("arial", 24)

# Spawn asteroids
def spawn_asteroid(size, x=None, y=None):
    asteroid = {
        "x": x or random.randint(0, WIDTH),
        "y": y or random.randint(0, HEIGHT),
        "dx": (random.random() - 0.5) * ASTEROID_SPEED,
        "dy": (random.random() - 0.5) * ASTEROID_SPEED,
        "radius": size
    }
    if math.hypot(asteroid["x"] - ship["x"], asteroid["y"] - ship["y"]) < asteroid["radius"] + ship["radius"] + 50:
        asteroid["x"] = random.randint(0, WIDTH)
        asteroid["y"] = random.randint(0, HEIGHT)
    asteroids.append(asteroid)

# Spawn UFO
def spawn_ufo():
    global ufo
    side = random.choice([-1, 1])
    x = 0 if side == 1 else WIDTH
    ufo = {
        "x": x,
        "y": random.randint(100, HEIGHT - 100),
        "dx": side * UFO_SPEED,
        "radius": UFO_RADIUS,
        "shoot_timer": UFO_SHOOT_INTERVAL
    }

# Initialize asteroids
for _ in range(ASTEROID_COUNT):
    spawn_asteroid(ASTEROID_SIZES[0])

# Game loop
running = True
space_pressed = False
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            keys.add(event.key)
        elif event.type == pygame.KEYUP:
            keys.discard(event.key)

    # Update ship
    if pygame.K_LEFT in keys:
        ship["angle"] += ROTATION_SPEED
    if pygame.K_RIGHT in keys:
        ship["angle"] -= ROTATION_SPEED
    ship["thrusting"] = pygame.K_UP in keys
    if ship["thrusting"]:
        ship["dx"] += math.cos(math.radians(ship["angle"])) * SHIP_SPEED
        ship["dy"] -= math.sin(math.radians(ship["angle"])) * SHIP_SPEED
    ship["x"] += ship["dx"]
    ship["y"] += ship["dy"]
    ship["dx"] *= FRICTION
    ship["dy"] *= FRICTION

    # Wrap ship
    ship["x"] %= WIDTH
    ship["y"] %= HEIGHT

    # Shoot
    if pygame.K_SPACE in keys and not space_pressed:
        bullets.append({
            "x": ship["x"] + math.cos(math.radians(ship["angle"])) * ship["radius"],
            "y": ship["y"] - math.sin(math.radians(ship["angle"])) * ship["radius"],
            "dx": math.cos(math.radians(ship["angle"])) * BULLET_SPEED + ship["dx"],
            "dy": -math.sin(math.radians(ship["angle"])) * BULLET_SPEED + ship["dy"],
            "life": 60
        })
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
            if lives <= 0:
                running = False

    # Update asteroids
    for asteroid in asteroids[:]:
        asteroid["x"] += asteroid["dx"]
        asteroid["y"] += asteroid["dy"]
        asteroid["x"] %= WIDTH
        asteroid["y"] %= HEIGHT
        for bullet in bullets[:]:
            if math.hypot(bullet["x"] - asteroid["x"], bullet["y"] - asteroid["y"]) < asteroid["radius"]:
                bullets.remove(bullet)
                score += 100 * (ASTEROID_SIZES.index(asteroid["radius"]) + 1)
                if asteroid["radius"] > ASTEROID_SIZES[2]:
                    new_size = ASTEROID_SIZES[ASTEROID_SIZES.index(asteroid["radius"]) + 1]
                    spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                    spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                asteroids.remove(asteroid)
                break
        if math.hypot(ship["x"] - asteroid["x"], ship["y"] - asteroid["y"]) < ship["radius"] + asteroid["radius"]:
            lives -= 1
            ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
            ship["dx"], ship["dy"] = 0, 0
            if lives <= 0:
                running = False

    # Update UFO
    if ufo is not None:
        ufo["x"] += ufo["dx"]
        if ufo["x"] < -ufo["radius"] or ufo["x"] > WIDTH + ufo["radius"]:
            ufo = None
        else:
            ufo["shoot_timer"] -= 1
            if ufo["shoot_timer"] <= 0:
                angle = math.atan2(ship["y"] - ufo["y"], ship["x"] - ufo["x"])
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
                    score += 1000
                    ufo = None
                    break
            if ufo is not None and math.hypot(ship["x"] - ufo["x"], ship["y"] - ufo["y"]) < ship["radius"] + ufo["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                ufo = None
                if lives <= 0:
                    running = False

    # Spawn UFO
    ufo_spawn_timer -= 1
    if ufo_spawn_timer <= 0 and ufo is None:
        spawn_ufo()
        ufo_spawn_timer = random.randint(UFO_SPAWN_MIN, UFO_SPAWN_MAX)

    # Draw
    screen.fill(BLACK)
    ship_points = [
        (ship["x"] + math.cos(math.radians(ship["angle"])) * ship["radius"], ship["y"] - math.sin(math.radians(ship["angle"])) * ship["radius"]),
        (ship["x"] + math.cos(math.radians(ship["angle"] + 140)) * ship["radius"], ship["y"] - math.sin(math.radians(ship["angle"] + 140)) * ship["radius"]),
        (ship["x"] + math.cos(math.radians(ship["angle"] - 140)) * ship["radius"], ship["y"] - math.sin(math.radians(ship["angle"] - 140)) * ship["radius"])
    ]
    pygame.draw.polygon(screen, WHITE, ship_points, 1)
    if ship["thrusting"]:
        thrust_point = (
            ship["x"] + math.cos(math.radians(ship["angle"] + 180)) * ship["radius"] * 1.5,
            ship["y"] - math.sin(math.radians(ship["angle"] + 180)) * ship["radius"] * 1.5
        )
        pygame.draw.line(screen, ORANGE, ship_points[1], thrust_point, 2)
        pygame.draw.line(screen, ORANGE, ship_points[2], thrust_point, 2)

    for bullet in bullets:
        pygame.draw.circle(screen, RED, (int(bullet["x"]), int(bullet["y"])), 2)

    for bullet in enemy_bullets:
        pygame.draw.circle(screen, GREEN, (int(bullet["x"]), int(bullet["y"])), 2)

    for asteroid in asteroids:
        pygame.draw.circle(screen, WHITE, (int(asteroid["x"]), int(asteroid["y"])), asteroid["radius"], 1)

    if ufo is not None:
        ufo_points = [
            (ufo["x"] - ufo["radius"], ufo["y"] + ufo["radius"]),
            (ufo["x"] + ufo["radius"], ufo["y"] + ufo["radius"]),
            (ufo["x"] + ufo["radius"] * 1.5, ufo["y"]),
            (ufo["x"] + ufo["radius"], ufo["y"] - ufo["radius"]),
            (ufo["x"] - ufo["radius"], ufo["y"] - ufo["radius"]),
            (ufo["x"] - ufo["radius"] * 1.5, ufo["y"])
        ]
        pygame.draw.polygon(screen, WHITE, ufo_points, 1)

    score_text = font.render(f"Score: {score}", True, WHITE)
    lives_text = font.render(f"Lives: {lives}", True, WHITE)
    screen.blit(score_text, (10, 10))
    screen.blit(lives_text, (10, 40))

    if lives <= 0:
        game_over_text = font.render("Game Over! Press R to Restart", True, WHITE)
        screen.blit(game_over_text, (WIDTH // 2 - 100, HEIGHT // 2))
        pygame.display.flip()
        while lives <= 0:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                    lives = 1
                elif event.type == pygame.KEYDOWN and event.key == pygame.K_r:
                    ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                    ship["dx"], ship["dy"] = 0, 0
                    ship["angle"] = 0
                    asteroids.clear()
                    bullets.clear()
                    enemy_bullets.clear()
                    ufo = None
                    score = 0
                    lives = 3
                    ufo_spawn_timer = random.randint(UFO_SPAWN_MIN, UFO_SPAWN_MAX)
                    for _ in range(ASTEROID_COUNT):
                        spawn_asteroid(ASTEROID_SIZES[0])
                    break
            if not running:
                break
        continue

    pygame.display.flip()
    clock.tick(60)

pygame.quit()
