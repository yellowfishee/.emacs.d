from PyQt6.QtGui import QColor, QPen
from PyQt6.QtCore import QPoint, QTimer, Qt, QPointF, QRectF
from PyQt6.QtGui import QPainter, QRadialGradient
from PyQt6.QtWidgets import QGraphicsView, QGraphicsScene, QGraphicsItem
from PyQt6.QtGui import QPixmap
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QGraphicsTextItem

import math
import random
from utils import *

ROMANTIC_COLORS = [
    '#FF6B6B', # Coral red
    '#4ECDC4', # Mint green
    '#45B7D1', # Sky blue
    '#FFA07A', # Light salmon
    '#98D8C8', # Sea foam
    '#F7E8A6', # Champagne
    '#AED9E0', # Powder blue
    '#FAD02E', # Sunflower yellow
    '#B19CD9', # Pale purple
    '#FF90B3', # Pink
    '#7FBC8C', # Sage green
    '#C5E99B', # Soft green
    '#F9D5E5', # Light pink
    '#E6E6FA', # Lavender
    '#FFD700', # Gold
]

class HexDigit(QGraphicsTextItem):
    def __init__(self, pos, digit, direction):
        super().__init__(digit)
        self.setPos(pos)
        self.direction = direction
        self.speed = random.uniform(1, 3)
        self.opacity = 1.0
        self.lifetime = random.randint(20, 40)  # Frames the digit will live

        font = QFont("Courier", 14)
        font.setBold(True)
        self.setFont(font)
        self.setDefaultTextColor(QColor(0, 255, 0))

    def advance(self):
        new_pos = self.pos() + self.direction * self.speed
        self.setPos(new_pos)
        self.opacity -= 1 / self.lifetime
        color = self.defaultTextColor()
        color.setAlphaF(self.opacity)
        self.setDefaultTextColor(color)
        self.lifetime -= 1
        return self.lifetime > 0

class HexBurstEffect(QGraphicsItem):
    def __init__(self, x, y, width, height):
        super().__init__()
        self.setPos(x, y - 120)
        self.bounds = QRectF(0, 0, width, height)
        self.digits = []
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_digits)
        self.timer.start(33)  # Update about 30 times per second

        self.create_burst()

        QTimer.singleShot(2000, self.remove_effect)  # Remove effect after 2 seconds

    def create_burst(self):
        center = QPointF(self.bounds.width() / 2, self.bounds.height() / 2)
        hex_chars = "0123456789ABCDEF"
        for _ in range(20):  # Create 20 digits
            angle = random.uniform(0, 2 * math.pi)
            direction = QPointF(math.cos(angle), math.sin(angle))
            digit = HexDigit(center, random.choice(hex_chars), direction)
            digit.setParentItem(self)
            self.digits.append(digit)

    def boundingRect(self):
        return self.bounds

    def paint(self, painter, option, widget):
        # The effect itself is invisible, only digits are visible
        pass

    def update_digits(self):
        self.digits = [digit for digit in self.digits if digit.advance()]
        if not self.digits:
            self.remove_effect()

    def remove_effect(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)
        self.timer.stop()

class MatrixRainDrop(QGraphicsTextItem):
    def __init__(self, x, height):
        super().__init__()
        self.setPos(x, 0)
        self.height = height
        self.speed = random.uniform(1, 3)
        self.setPlainText(random.choice("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+-=[]{}|;:,.<>?"))
        self.setDefaultTextColor(QColor(0, 255, 0, 255))  # Bright green
        self.setFont(QFont("Courier", 12))
        self.lifetime = random.randint(10, 50)  # Number of frames the drop will live

    def advance(self):
        new_y = self.y() + self.speed
        self.setY(new_y)
        self.lifetime -= 1
        if random.random() < 0.1:  # 10% chance to change character
            self.setPlainText(random.choice("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+-=[]{}|;:,.<>?"))
        if self.lifetime <= 0 or new_y > self.height:
            return False
        return True

class MatrixRainEffect(QGraphicsItem):
    def __init__(self, x, y, width, height):
        super().__init__()
        self.setPos(x, y - 120)
        self.bounds = QRectF(0, 0, width, height)
        self.drops = []
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_drops)
        self.timer.start(10)  # Update about 30 times per second

        self.opacity = 1.0
        QTimer.singleShot(300, self.start_fading)  # Start fading after 5 seconds
        QTimer.singleShot(500, self.remove_effect)  # Remove effect after 7.5 seconds

    def boundingRect(self):
        return self.bounds

    def paint(self, painter, option, widget):
        # The effect itself is invisible, only drops are visible
        pass

    def update_drops(self):
        # Add new drops
        if len(self.drops) < 50 and random.random() < 0.3:
            new_drop = MatrixRainDrop(random.uniform(0, self.bounds.width()), self.bounds.height())
            new_drop.setParentItem(self)
            self.drops.append(new_drop)

        # Update existing drops
        self.drops = [drop for drop in self.drops if drop.advance()]

    def start_fading(self):
        self.fade_timer = QTimer()
        self.fade_timer.timeout.connect(self.fade_out)
        self.fade_timer.start(50)  # Update fade every 50ms

    def fade_out(self):
        self.opacity -= 0.02
        if self.opacity < 0:
            self.opacity = 0
        for drop in self.drops:
            color = drop.defaultTextColor()
            color.setAlpha(int(255 * self.opacity))
            drop.setDefaultTextColor(color)
        self.update()

    def remove_effect(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)
        self.timer.stop()
        if hasattr(self, 'fade_timer'):
            self.fade_timer.stop()

class Firefly(QGraphicsItem):
    def __init__(self, bounds):
        super().__init__()
        self.bounds = bounds
        self.setPos(random.uniform(bounds.left(), bounds.right()),
                    random.uniform(bounds.top(), bounds.bottom()))
        self.color = QColor(random.choice(ROMANTIC_COLORS))

        self.size = random.uniform(2, 4)
        self.opacity = random.uniform(0.3, 1.0)
        self.direction = random.uniform(0, 2 * math.pi)
        self.speed = random.uniform(0.5, 1.5)
        self.blink_speed = random.uniform(0.02, 0.05)
        self.blink_direction = 1

    def boundingRect(self):
        return QRectF(-self.size, -self.size, self.size * 2, self.size * 2)

    def paint(self, painter, option, widget):
        gradient = QRadialGradient(0, 0, self.size)
        color = self.color.toRgb()
        color.setAlpha(int(255 * self.opacity))
        gradient.setColorAt(0, color)
        gradient.setColorAt(1, Qt.GlobalColor.transparent)
        painter.setBrush(gradient)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        # Move the firefly
        new_pos = self.pos() + QPointF(math.cos(self.direction) * self.speed,
                                       math.sin(self.direction) * self.speed)

        # If firefly goes out of bounds, wrap around to the other side
        if not self.bounds.contains(new_pos):
            if new_pos.x() < self.bounds.left():
                new_pos.setX(self.bounds.right())
            elif new_pos.x() > self.bounds.right():
                new_pos.setX(self.bounds.left())
            if new_pos.y() < self.bounds.top():
                new_pos.setY(self.bounds.bottom())
            elif new_pos.y() > self.bounds.bottom():
                new_pos.setY(self.bounds.top())

        self.setPos(new_pos)

        # Randomly change direction
        if random.random() < 0.05:  # 5% chance to change direction each frame
            self.direction = random.uniform(0, 2 * math.pi)

        # Blink effect
        self.opacity += self.blink_direction * self.blink_speed
        if self.opacity > 1.0 or self.opacity < 0.3:
            self.blink_direction *= -1

        self.update()
        return True

class FireflyEffect(QGraphicsItem):
    def __init__(self, x, y, width, height):
        super().__init__()
        self.setPos(x, y - 50)
        self.bounds = QRectF(0, 0, width, height)
        self.fireflies = [Firefly(self.bounds) for _ in range(10)] # Create fireflies number
        for firefly in self.fireflies:
            firefly.setParentItem(self)

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_fireflies)
        self.timer.start(33)  # Update about 30 times per second

        self.opacity = 1.0
        QTimer.singleShot(500, self.start_fading)  # Start fading after 5 seconds
        QTimer.singleShot(700, self.remove_effect)  # Remove effect after 7.5 seconds (5s display + 2.5s fade)

    def boundingRect(self):
        return self.bounds

    def paint(self, painter, option, widget):
        # The effect itself is invisible, only fireflies are visible
        pass

    def update_fireflies(self):
        for firefly in self.fireflies:
            firefly.advance()

    def start_fading(self):
        self.fade_timer = QTimer()
        self.fade_timer.timeout.connect(self.fade_out)
        self.fade_timer.start(50)  # Update fade every 50ms

    def fade_out(self):
        self.opacity -= 0.02
        if self.opacity < 0:
            self.opacity = 0
        for firefly in self.fireflies:
            firefly.opacity = min(firefly.opacity, self.opacity)
        self.update()

    def remove_effect(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)
        self.timer.stop()
        if hasattr(self, 'fade_timer'):
            self.fade_timer.stop()

class WaterDroplet(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.size = random.uniform(2, 6)
        angle = random.uniform(0, 2 * math.pi)
        speed = random.uniform(1, 3)
        self.velocity = QPointF(speed * math.cos(angle), speed * math.sin(angle) - 2)
        self.opacity = 1.0

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.velocity += QPointF(0, 0.1)  # Gravity effect
        self.opacity -= 0.02
        self.size -= 0.05
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class WaterSplash(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(16)  # 60 FPS

        self.create_particles()
        QTimer.singleShot(1000, self.remove_splash)

    def boundingRect(self):
        return QRectF(-50, -50, 100, 100)

    def paint(self, painter, option, widget):
        # The splash itself is invisible, only particles are visible
        pass

    def create_particles(self):
        for _ in range(20):
            color = QColor(random.choice(['#4FC3F7', '#29B6F6', '#03A9F4', '#039BE5', '#0288D1']))
            particle = WaterDroplet(color)
            particle.setParentItem(self)
            self.particles.append(particle)

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]

    def remove_splash(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

class BalloonParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-0.5, 0.5), random.uniform(-2, -1))
        self.opacity = 1.0
        self.size = random.uniform(10, 20)
        self.string_opacity = 1.0
        self.string_length = self.size * 1.5  # 增加线的长度

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size + self.string_length)

    def paint(self, painter, option, widget):
        # 绘制气球
        painter.setPen(QPen(self.color.darker(), 1))
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(QRectF(-self.size/2, -self.size/2, self.size, self.size))

        # 绘制气球线
        painter.setPen(QPen(Qt.GlobalColor.black, 1))
        painter.setOpacity(self.string_opacity)
        painter.drawLine(QPointF(0, self.size/2), QPointF(0, self.size/2 + self.string_length))

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.01
        self.string_opacity -= 0.015  # 线比气球消失得稍快
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Balloon(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(1000, self.remove_balloon)

    def boundingRect(self):
        return QRectF(-30, -60, 60, 60)

    def paint(self, painter, option, widget):
        # Balloon itself is invisible, only particles are visible
        pass

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        if len(self.particles) < 5:
            color = QColor(random.choice(ROMANTIC_COLORS))

            particle = BalloonParticle(color)
            particle.setPos(random.uniform(-20, 20), random.uniform(-20, 20))
            particle.setParentItem(self)
            self.particles.append(particle)

    def remove_balloon(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)


class LightningBolt(QGraphicsItem):
    def __init__(self, start, end, branch_probability=0.3):
        super().__init__()
        self.start = start
        self.end = end
        self.branch_probability = branch_probability
        self.segments = []
        self.generate_lightning()
        self.opacity = 1.0

        self.pixmap = None
        self.bounding_rect = None
        self.create_pixmap()

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_lightning)
        self.timer.start(10)

    def generate_lightning(self):
        self.segments = []
        self.create_segment(self.start, self.end)

    def create_segment(self, start, end):
        if (end - start).manhattanLength() < 10:
            self.segments.append((start, end))
            return

        mid = QPointF((start.x() + end.x()) / 2, (start.y() + end.y()) / 2)
        displacement = QPointF(-(end.y() - start.y()), end.x() - start.x())
        displacement *= (random.random() * 0.4 - 0.2)
        mid += displacement

        self.create_segment(start, mid)
        self.create_segment(mid, end)

        if random.random() < self.branch_probability:
            branch_end = mid + (mid - start) * 0.7
            branch_end += QPointF(random.random() * 20 - 10, random.random() * 20 - 10)
            self.create_segment(mid, branch_end)

    def boundingRect(self):
        return self.bounding_rect

    def create_pixmap(self):
        rect = QRectF(self.start, self.end).normalized().adjusted(-20, -20, 20, 20)
        self.bounding_rect = rect
        self.pixmap = QPixmap(rect.size().toSize())
        self.pixmap.fill(Qt.GlobalColor.transparent)

        painter = QPainter(self.pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        painter.translate(-rect.topLeft())

        main_color = QColor(200, 220, 255, 255)
        painter.setPen(QPen(main_color, 3, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap, Qt.PenJoinStyle.RoundJoin))
        for start, end in self.segments:
            painter.drawLine(start, end)

        glow_color = QColor(220, 240, 255, 100)
        painter.setPen(QPen(glow_color, 6, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap, Qt.PenJoinStyle.RoundJoin))
        for start, end in self.segments:
            painter.drawLine(start, end)

        painter.end()

    def paint(self, painter, option, widget):
        painter.setOpacity(self.opacity)
        painter.drawPixmap(self.boundingRect().topLeft(), self.pixmap)

    def update_lightning(self):
        self.opacity -= 0.05
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            self.timer.stop()
        self.update()

class SupernovaParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        angle = random.uniform(0, 2 * 3.14159)
        speed = random.uniform(1, 3)
        self.velocity = QPointF(speed * math.cos(angle), speed * math.sin(angle))
        self.opacity = 1.0
        self.size = 4

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        self.size -= 0.05
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Supernova(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.particles = []
        self.core_size = 30
        self.core_opacity = 1.0

        self.setPos(x, y - 50)

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_supernova)
        self.timer.start(10)

        QTimer.singleShot(300, self.remove_supernova)

    def boundingRect(self):
        return QRectF(-100, -100, 200, 200)

    def update_supernova(self):
        # Update existing particles
        self.particles = [p for p in self.particles if p.advance()]

        # Create new particles
        for _ in range(5):
            color = QColor(random.choice(['red', 'yellow', 'orange', 'white']))
            particle = SupernovaParticle(color)
            particle.setPos(0, 0)
            particle.setParentItem(self)
            self.particles.append(particle)

        # Update core
        self.core_size += 0.5
        self.core_opacity -= 0.01
        if self.core_opacity < 0:
            self.core_opacity = 0

        self.update()

    def remove_supernova(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

class Particle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-1, 1), random.uniform(-1, 1))
        self.opacity = 1.0

    def boundingRect(self):
        return QRectF(-1.5, -1.5, 3, 3)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Firework(QGraphicsItem):
    def __init__(self, x, y, color):
        super().__init__()
        self.size = 20

        self.setPos(x, y + self.size * 2)

        self.particles = [Particle(color) for _ in range(30)]
        for particle in self.particles:
            particle.setParentItem(self)

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(2000, self.remove_firework)

    def boundingRect(self):
        return QRectF(-self.size, -self.size, self.size * 2, self.size * 2)

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        if not self.particles:
            self.remove_firework()

    def remove_firework(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

class FlameParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-0.5, 0.5), random.uniform(-2, -1))
        self.opacity = 1.0
        self.size = random.uniform(3, 6)

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        gradient = QRadialGradient(QPointF(0, 0), self.size/2)
        gradient.setColorAt(0, self.color)
        gradient.setColorAt(1, Qt.GlobalColor.transparent)

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(gradient)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        self.size -= 0.1
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Flame(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(400, self.remove_flame)

    def boundingRect(self):
        return QRectF(-20, -40, 40, 40)

    def paint(self, painter, option, widget):
        # Flame itself is invisible, only particles are visible
        pass

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        for _ in range(3):
            color = QColor(random.choice(['red', 'yellow', '#FFA500']))  # Using '#FFA500' for orange
            particle = FlameParticle(color)
            particle.setPos(random.uniform(-10, 10), 0)
            particle.setParentItem(self)
            self.particles.append(particle)

    def remove_flame(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)


class TypeAnimationScene(QGraphicsScene):
    def start_animation(self, x, y, style):
        if style == "firework":
            color = QColor(random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))
            firework = Firework(x, y, color)
            self.addItem(firework)
        elif style == "water":
            splash = WaterSplash(x, y)
            self.addItem(splash)
        elif style == "matrix":
            matrix_effect = MatrixRainEffect(x, y, 200, 200)  # 调整大小
            self.addItem(matrix_effect)
        elif style == "hex":
            hex_effect = HexBurstEffect(x, y, 100, 100)  # 调整大小
            self.addItem(hex_effect)
        elif style == "firefly":
            firefly_effect = FireflyEffect(x, y, 50, 50)  # Adjust size as needed
            self.addItem(firefly_effect)
        elif style == "flame":
            flame = Flame(x, y)
            self.addItem(flame)
        elif style == "balloon":
            balloon = Balloon(x, y)
            self.addItem(balloon)
        elif style == "lightning":
            angle = random.uniform(0, 2 * math.pi)
            length = random.uniform(100, 200)
            end_x = x + length * math.cos(angle)
            end_y = y + length * math.sin(angle)
            lightning = LightningBolt(QPointF(x, y), QPointF(end_x, end_y))
            self.addItem(lightning)
        elif style == "supernova":
            supernova = Supernova(x, y)
            self.addItem(supernova)

class TypeAnimation(QGraphicsView):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.firework_scene = TypeAnimationScene()
        self.setScene(self.firework_scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Make sure firework render coordinate same as cursor coordinate.
        self.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)

        (self.enable_type_animation, self.type_style) = get_emacs_vars([
            "holo-layer-enable-type-animation",
            "holo-layer-type-animation-style"
        ])

    def show(self, x, y):
        # It's important, we need call mapToScene to make sure firework at correct coordinate.
        point = QPoint(int(x), int(y))
        scene_pos = self.mapToScene(point)
        self.firework_scene.start_animation(scene_pos.x(), scene_pos.y(), self.type_style)
