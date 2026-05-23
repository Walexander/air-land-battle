import bpy

count = 0
for obj in bpy.data.objects:
    if hasattr(obj, "skein_two") and len(obj.skein_two) > 0:
        obj.skein_two.clear()
        count += 1
    if "skein" in obj.keys():
        del obj["skein"]
    if "skein_two" in obj.keys():
        del obj["skein_two"]

for mesh in bpy.data.meshes:
    if hasattr(mesh, "skein_two") and len(mesh.skein_two) > 0:
        mesh.skein_two.clear()
        count += 1
    if "skein" in mesh.keys():
        del mesh["skein"]
    if "skein_two" in mesh.keys():
        del mesh["skein_two"]

print(f"Cleared skein data from {count} objects/meshes")
