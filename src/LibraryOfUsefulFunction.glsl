

float symmetricalSmoothStep(float apex, float width, float value) {
  return smoothstep(apex - width, apex, value) - smoothstep(apex, apex + width, value);
}


float rectangle(vec2 position, vec2 size, vec2 value) {
  vec2 bl = step(position, value);
  vec2 tr = step((position+size), value);
  return bl.x * bl.y * (1.0 - tr.x) * (1.0 - tr.y);
}
