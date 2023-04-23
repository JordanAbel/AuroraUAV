import tkinter as tk
from tkinter import filedialog
from tkinter.font import Font
from PIL import ImageTk, Image
import subprocess, os, sys
from threading import Thread, Event

bg_col = '#1c1c1c'

class MainWindow(tk.Frame):
	root_path = getattr(sys, '_MEIPASS', os.getcwd())
	
	def __init__(self, master=None):
		super().__init__(master)
		self.master = master
		self.grid()
		self.create_gui()
		self.r_thread = None
		self.event = Event()
	
	def create_gui(self):
		
		# TODO: delete these paths after testing
		o_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif"
		pc_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/points.las"
		save_dir_path = "/Users/Shea/Desktop"
		
		title_font = Font(family="Helvetica", size=26, weight="bold")
		
		# add image background
		self.image_path = os.path.join(self.root_path, 'forest_bg1.png')
		self.image = Image.open(self.image_path)
		self.background_image = ImageTk.PhotoImage(self.image)
		self.background_label = tk.Label(self.master, image=self.background_image)
		self.background_label.place(x=0, y=0, relwidth=1, relheight=1)
		self.master.geometry("{}x{}".format(self.image.size[0], self.image.size[1]))
		
		# create containers for individual rows
		title_container = tk.Frame(root, bg=bg_col)
		ver_container = tk.Frame(root, bg=bg_col)
		browse_container = tk.Frame(root, bg=bg_col)
		tog_container = tk.Frame(root, bg=bg_col)
		in_container = tk.Frame(root, bg=bg_col)
		btn_container = tk.Frame(root, bg=bg_col)
		text_container = tk.Frame(root, bg=bg_col)
		
		# create title and version number
		self.title = tk.Label(title_container, text="Forest Metrics Software", font=title_font, bg=bg_col)
		self.title.grid(row=0, column=0, sticky="nsew")
		self.ver = tk.Label(ver_container, text="v 2.1.0", bg=bg_col)
		self.ver.grid(row=0, column=0)
		
		# create file browsers
		self.ortho_label = tk.Label(browse_container, text="Orthomosaic:", bg=bg_col)
		self.ortho_path = tk.Label(browse_container, text=o_path, bg=bg_col)  # TODO: delete and uncomment below line
		# self.ortho_path = tk.Label(browse_container, text="No Orthomosaic (.TIF) selected", bg=bg_col)
		self.ortho_button = tk.Button(browse_container, text="Browse", command=self.browse_ortho, bg=bg_col)
		self.ortho_label.grid(row=0, column=0, sticky="nsew")
		self.ortho_path.grid(row=0, column=1, sticky="nsew")
		self.ortho_button.grid(row=0, column=2, sticky="nsew")
		
		self.pc_label = tk.Label(browse_container, text="Point Cloud:", bg=bg_col)
		self.pc_path = tk.Label(browse_container, text=pc_path, bg=bg_col)  # TODO: Delete and uncomment below line
		# self.pc_path = tk.Label(browse_container, text="No Point Cloud (.LAS) selected", bg=bg_col)
		self.pc_button = tk.Button(browse_container, text="Browse", command=self.browse_pc, bg=bg_col)
		self.pc_label.grid(row=1, column=0, sticky="nsew")
		self.pc_path.grid(row=1, column=1, sticky="nsew")
		self.pc_button.grid(row=1, column=2, sticky="nsew")
		
		self.save_dir_label = tk.Label(browse_container, text="Output Directory:", bg=bg_col)
		self.save_dir_path = tk.Label(browse_container, text=save_dir_path, bg=bg_col)  # TODO: delete and uncomment below line
		# self.save_dir_path = tk.Label(browse_container, text="No output directory selected", bg=bg_col)
		self.save_dir_button = tk.Button(browse_container, text="Browse", command=self.browse_sd, bg=bg_col)
		self.save_dir_label.grid(row=2, column=0, sticky="nsew")
		self.save_dir_path.grid(row=2, column=1, sticky="nsew")
		self.save_dir_button.grid(row=2, column=2, sticky="nsew")
		
		# create plot toggles
		self.toggle_label = tk.Label(tog_container, text="Output Plots:", bg=bg_col)
		self.toggle_label.grid(row=1, column=0, columnspan=8, sticky="nsew")
		
		# 2D Cross section
		self.cross_var = tk.BooleanVar(value=False)
		self.cross_button = tk.Checkbutton(tog_container, text="Cross", variable=self.cross_var, bg=bg_col)
		self.cross_button.grid(row=2, column=0)
		Tooltip(self.cross_button, "2D plot of cross-section of terrain")
		
		# 3d Digital terrain model
		self.dtm_var = tk.BooleanVar(value=False)
		self.dtm_button = tk.Checkbutton(tog_container, text="DTM", variable=self.dtm_var, bg=bg_col)
		self.dtm_button.grid(row=2, column=1)
		Tooltip(self.dtm_button, "3D plot of digital terrain model")
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.n_normalized_var = tk.BooleanVar(value=False)
		self.n_normalized_button = tk.Checkbutton(tog_container, text="Non-norm", variable=self.n_normalized_var, bg=bg_col)
		self.n_normalized_button.grid(row=2, column=2)
		Tooltip(self.n_normalized_button, "3D plot of non-normalized \n"
		                                  "(non-flattened) point cloud in heat map colouring")
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.normalized_var = tk.BooleanVar(value=False)
		self.normalized_button = tk.Checkbutton(tog_container, text="Norm", variable=self.normalized_var, bg=bg_col)
		self.normalized_button.grid(row=2, column=3)
		Tooltip(self.normalized_button, "3D plot of normalized (flattened) point cloud in heat map colouring")
		
		# 3D rgb point cloud
		self.rgb_var = tk.BooleanVar(value=False)
		self.rgb_button = tk.Checkbutton(tog_container, text="RGB", variable=self.rgb_var, bg=bg_col)
		self.rgb_button.grid(row=2, column=4)
		Tooltip(self.rgb_button, "3D plot of normalized (flattened) point cloud in RGB colouring")
		
		# 2D located trees
		self.canopy_var = tk.BooleanVar(value=False)
		self.canopy_button = tk.Checkbutton(tog_container, text="Canopy", variable=self.canopy_var, bg=bg_col)
		self.canopy_button.grid(row=2, column=5)
		Tooltip(self.canopy_button, "2D plot of overhead view of tree canopy with crown markers")
		
		# 3D segments
		self.segments_var = tk.BooleanVar(value=False)
		self.segments_button = tk.Checkbutton(tog_container, text="Segments", variable=self.segments_var, bg=bg_col)
		self.segments_button.grid(row=2, column=6)
		Tooltip(self.segments_button, "3D plot of automatically generated tree segments")
		
		# 2D segments - rgb overlay
		self.overlay_var = tk.BooleanVar(value=False)
		self.overlay_button = tk.Checkbutton(tog_container, text="Overlay", variable=self.overlay_var, bg=bg_col)
		self.overlay_button.grid(row=2, column=7)
		Tooltip(self.overlay_button, "2D plot of tree segments overlayed on orthomosaic map")
		
		# create inputs
		self.toggle_label = tk.Label(in_container, text="Point Cloud Tile Params:", bg=bg_col)
		self.toggle_label.grid(row=0, column=0, columnspan=2, sticky="nsew")
		
		self.tile_input_var = tk.StringVar(value="250")
		self.tile_size_label = tk.Label(in_container, text="Tile Size (m):", bg=bg_col)
		self.tile_size_label.grid(row=3, column=0)
		self.tile_size_entry = tk.Entry(in_container, textvariable=self.tile_input_var, bg=bg_col)
		self.tile_size_entry.grid(row=3, column=1)
		
		self.buff_input_var = tk.StringVar(value="0")
		self.tile_buffer_label = tk.Label(in_container, text="Tile Buffer (m):", bg=bg_col)
		self.tile_buffer_label.grid(row=4, column=0)
		self.tile_buffer_entry = tk.Entry(in_container, textvariable=self.buff_input_var, bg=bg_col)
		self.tile_buffer_entry.grid(row=4, column=1)
		
		# create options for tables
		# TODO: options for outputting 2 column table for manual labelling, etc
		
		# create run/ stop button
		self.run_button = tk.Button(btn_container, text="Run", command=self.run_segmentation, bg=bg_col)
		self.run_button.grid(row=0, column=1)
		self.stop_button = tk.Button(btn_container, text="Stop", command=self.stop_r_thread, bg=bg_col)
		self.stop_button.grid(row=0, column=2)
		
		# create output text box
		self.toggle_button = tk.Button(text_container, text="Show Output", command=self.toggle_textbox, bg=bg_col)
		self.toggle_button.grid(row=0, column=0)
		self.textbox = tk.Text(text_container, height=10, bg=bg_col)
		self.textbox.grid(row=1, column=0)
		
		# place containers on GUI
		title_container.place(relx=0.5, rely=0.15, anchor="center")
		ver_container.place(relx=0.95, rely=0.05, anchor="center")
		browse_container.place(relx=0.5, rely=0.27, anchor="center")
		tog_container.place(relx=0.5, rely=0.4, anchor="center")
		in_container.place(relx=0.5, rely=0.53, anchor="center")
		btn_container.place(relx=0.5, rely=0.63, anchor="center")
		text_container.place(relx=0.5, rely=0.855, anchor="center")
		self.textbox.grid_remove()
	
	def browse_ortho(self):
		self.ortho_p = filedialog.askopenfilename()
		self.ortho_path.config(text=self.ortho_p)
	
	def browse_pc(self):
		self.pc_p = filedialog.askopenfilename()
		self.pc_path.config(text=self.pc_p)
	
	def browse_sd(self):
		self.sd_p = filedialog.askdirectory()
		self.save_dir_path.config(text=self.sd_p)
	
	def toggle_textbox(self):
		# toggle the visibility of the text box
		if self.textbox.winfo_ismapped():
			self.textbox.grid_remove()
			self.toggle_button.config(text="Show Output")
		else:
			self.textbox.grid()
			self.toggle_button.config(text="Hide")
	
	def stop_r_thread(self):
		self.event.set()
		self.run_button.config(state="normal")
	
	def run_segmentation(self):
		self.event.clear()
		self.run_button.config(state="disabled")
		self.textbox.delete("1.0", tk.END)
		self.textbox.grid()
		self.textbox.insert(tk.END, "Processing Starting\n")
		self.toggle_button.config(text="Hide")
		# Get the input values from the GUI
		# Paths
		ortho_path = self.ortho_path.cget("text")
		pc_path = self.pc_path.cget("text")
		sd_path = self.save_dir_path.cget("text")
		# Inputs
		tile_size = int(self.tile_size_entry.get())
		tile_buffer = int(self.tile_buffer_entry.get())
		# Toggles
		cross = self.cross_var.get()
		dtm = self.dtm_var.get()
		n_normalized = self.n_normalized_var.get()
		normalized = self.normalized_var.get()
		rgb = self.rgb_var.get()
		canopy = self.canopy_var.get()
		segments = self.segments_var.get()
		overlay = self.overlay_var.get()
		
		# Input checks before running script
		# Checks to do:
		# - Save dir path
		# - extreme tile/ buffer sizes
		# - file type of input files
		# - No plots selected
		
		if tile_size < 1 or tile_buffer < 0:
			# Display an error message
			tk.messagebox.showerror("Error",
			                        "Tile size must be greater than 0.\nBuffer size must be greater than or equal to 0")
			self.run_button.config(state="normal")
			return
		if ortho_path == "No Orthomosaic (.TIF) selected":
			# Display an error message
			tk.messagebox.showerror("Error", "Orthomosaic file not selected")
			self.run_button.config(state="normal")
			return
		if pc_path == "No Point Cloud (.LAS) selected":
			# Display an error message
			tk.messagebox.showerror("Error", "Point Cloud file not selected")
			self.run_button.config(state="normal")
			return
		
		else:
			self.r_script = os.path.join(self.root_path, 'tree_segmentation_v2.R')
			
			# Make the R script executable (equivalent to running "chmod +x r_script" in terminal)
			os.chmod(self.r_script, 0o755)
			
			# Set the arguments to pass to the R script
			cmd = ["Rscript", self.r_script]
			args = [str(ortho_path), str(pc_path), str(sd_path), str(self.root_path),
			        str(tile_size), str(tile_buffer), str(cross), str(dtm), str(n_normalized),
			        str(normalized), str(rgb), str(canopy), str(segments), str(overlay)]
			
			# Create and start R thread
			r_thread = Thread(target=self.run_script, args=(cmd, args, self.event))
			r_thread.start()
	
	def run_script(self, cmd, args, event):
		cmd = cmd + args if args else cmd
		try:
			process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
			
			while True:
				output = process.stdout.readline()
				if output == b'' and process.poll() is not None:
					break
				if output:
					output = output.decode('utf-8')
					
					print(output)
					self.textbox.insert(tk.END, output)
					self.textbox.see(tk.END)
				if event.is_set():
					process.terminate()
					self.stop_r_thread()
					break
			
			process.wait()
			
			self.textbox.insert(tk.END, "Processing Terminated")
			self.textbox.see(tk.END)
			self.run_button.config(state="normal")
		
		except subprocess.CalledProcessError as e:
			print("Error:", e.output.decode())


class Tooltip:
	def __init__(self, widget, text):
		self.widget = widget
		self.text = text
		self.widget.bind("<Enter>", self.show)
		self.widget.bind("<Leave>", self.hide)
		self.widget.bind("<ButtonPress>", self.hide)
	
	def show(self, event=None):
		x, y, cx, cy = self.widget.bbox("insert")
		x += self.widget.winfo_rootx() + 25
		y += self.widget.winfo_rooty() + 20
		self.tw = tk.Toplevel(self.widget)
		self.tw.wm_overrideredirect(True)
		self.tw.wm_geometry("+%d+%d" % (x, y))
		label = tk.Label(self.tw, text=self.text, justify="left", relief="solid", borderwidth=1, bg=bg_col)
		label.pack(ipadx=1)
	
	def hide(self, event=None):
		if self.tw:
			self.tw.destroy()


if __name__ == "__main__":
	root = tk.Tk()
	root.title("Aurora UAV: Forest Metrics")
	
	app = MainWindow(master=root)
	app.mainloop()



