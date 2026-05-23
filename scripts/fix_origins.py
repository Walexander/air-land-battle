import bpy

count = 0
for obj in bpy.data.objects:
    if obj.type == 'MESH':
        obj.select_set(True)
        count += 1
    else:
        obj.select_set(False)

bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY', center='MEDIAN')

for obj in bpy.data.objects:
    obj.select_set(False)

print(f"Set origin to geometry for {count} mesh objects")
