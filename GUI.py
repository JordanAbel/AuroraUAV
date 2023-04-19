import queue
import time
import tkinter as tk
from tkinter import filedialog, ttk
from tkinter.font import Font
from PIL import ImageTk, Image
import subprocess, os, threading, sys


class MainWindow(tk.Frame):
	root_path = getattr(sys, '_MEIPASS', os.getcwd())
	
	def __init__(self, master=None):
		super().__init__(master)
		self.master = master
		self.grid()
		self.create_gui()
		self.r_thread = None
	
	def create_gui(self):
		
		# TODO: delete these paths after testing
		o_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif"
		pc_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/points.las"
		save_dir_path = "/Users/Shea/Desktop"
		
		title_font = Font(family="Helvetica", size=26, weight="bold")
		
		# add image background
		self.image_path = "C:/Users/spart/Documents/AuroraUAV/forest_bg copy.png"
		self.image = Image.open(self.image_path)
		self.background_image = ImageTk.PhotoImage(self.image)
		self.background_label = tk.Label(self.master, image=self.background_image)
		self.background_label.place(x=0, y=0, relwidth=1, relheight=1)
		self.master.geometry("{}x{}".format(self.image.size[0], self.image.size[1]))
		
		# create containers for individual rows
		title_container = tk.Frame(root, bg="")
		ver_container = tk.Frame(root, bg="")
		browse_container = tk.Frame(root, bg="")
		tog_container = tk.Frame(root, bg="")
		in_container = tk.Frame(root, bg="")
		btn_container = tk.Frame(root, bg="")
		text_container = tk.Frame(root, bg="")
		
		# create title and version number
		self.title = tk.Label(title_container, text="Forest Metrics Software", font=title_font)
		self.title.grid(row=0, column=0, sticky="nsew")
		self.ver = tk.Label(ver_container, text="v 2.1.0")
		self.ver.grid(row=0, column=0)
		
		# create file browsers
		self.ortho_label = tk.Label(browse_container, text="Orthomosaic:")
		self.ortho_path = tk.Label(browse_container, text=o_path)  # TODO: delete and uncomment below line
		# self.ortho_path = tk.Label(browse_container, text="No Orthomosaic (.TIF) selected", bg="systemTransparent")
		self.ortho_button = tk.Button(browse_container, text="Browse", command=self.browse_ortho)
		self.ortho_label.grid(row=0, column=0, sticky="nsew")
		self.ortho_path.grid(row=0, column=1, sticky="nsew")
		self.ortho_button.grid(row=0, column=2, sticky="nsew")
		
		self.pc_label = tk.Label(browse_container, text="Point Cloud:")
		self.pc_path = tk.Label(browse_container, text=pc_path)  # TODO: Delete and uncomment below line
		# self.pc_path = tk.Label(browse_container, text="No Point Cloud (.LAS) selected", bg="systemTransparent")
		self.pc_button = tk.Button(browse_container, text="Browse", command=self.browse_pc)
		self.pc_label.grid(row=1, column=0, sticky="nsew")
		self.pc_path.grid(row=1, column=1, sticky="nsew")
		self.pc_button.grid(row=1, column=2, sticky="nsew")
		
		self.save_dir_label = tk.Label(browse_container, text="Output Directory:")
		self.save_dir_path = tk.Label(browse_container, text=save_dir_path)  # TODO: delete and uncomment below line
		# self.save_dir_path = tk.Label(browse_container, text="No output directory selected", bg="systemTransparent")
		self.save_dir_button = tk.Button(browse_container, text="Browse", command=self.browse_sd)
		self.save_dir_label.grid(row=2, column=0, sticky="nsew")
		self.save_dir_path.grid(row=2, column=1, sticky="nsew")
		self.save_dir_button.grid(row=2, column=2, sticky="nsew")
		
		# create plot toggles
		self.toggle_label = tk.Label(tog_container, text="Output Plots:")
		self.toggle_label.grid(row=1, column=0, columnspan=8, sticky="nsew")
		
		# 2D Cross section
		self.cross_var = tk.BooleanVar(value=False)
		self.cross_button = tk.Checkbutton(tog_container, text="Cross", variable=self.cross_var)
		self.cross_button.grid(row=2, column=0)
		Tooltip(self.cross_button, "2D plot of cross-section of terrain")
		
		# 3d Digital terrain model
		self.dtm_var = tk.BooleanVar(value=False)
		self.dtm_button = tk.Checkbutton(tog_container, text="DTM", variable=self.dtm_var)
		self.dtm_button.grid(row=2, column=1)
		Tooltip(self.dtm_button, "3D plot of digital terrain model")
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.n_normalized_var = tk.BooleanVar(value=False)
		self.n_normalized_button = tk.Checkbutton(tog_container, text="Non-norm", variable=self.n_normalized_var)
		self.n_normalized_button.grid(row=2, column=2)
		Tooltip(self.n_normalized_button, "3D plot of non-normalized \n"
		                                  "(non-flattened) point cloud in heat map colouring")
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.normalized_var = tk.BooleanVar(value=False)
		self.normalized_button = tk.Checkbutton(tog_container, text="Norm", variable=self.normalized_var)
		self.normalized_button.grid(row=2, column=3)
		Tooltip(self.normalized_button, "3D plot of normalized (flattened) point cloud in heat map colouring")
		
		# 3D rgb point cloud
		self.rgb_var = tk.BooleanVar(value=False)
		self.rgb_button = tk.Checkbutton(tog_container, text="RGB", variable=self.rgb_var)
		self.rgb_button.grid(row=2, column=4)
		Tooltip(self.rgb_button, "3D plot of normalized (flattened) point cloud in RGB colouring")
		
		# 2D located trees
		self.canopy_var = tk.BooleanVar(value=False)
		self.canopy_button = tk.Checkbutton(tog_container, text="Canopy", variable=self.canopy_var)
		self.canopy_button.grid(row=2, column=5)
		Tooltip(self.canopy_button, "2D plot of overhead view of tree canopy with crown markers")
		
		# 3D segments
		self.segments_var = tk.BooleanVar(value=False)
		self.segments_button = tk.Checkbutton(tog_container, text="Segments", variable=self.segments_var)
		self.segments_button.grid(row=2, column=6)
		Tooltip(self.segments_button, "3D plot of automatically generated tree segments")
		
		# 2D segments - rgb overlay
		self.overlay_var = tk.BooleanVar(value=False)
		self.overlay_button = tk.Checkbutton(tog_container, text="Overlay", variable=self.overlay_var)
		self.overlay_button.grid(row=2, column=7)
		Tooltip(self.overlay_button, "2D plot of tree segments overlayed on orthomosaic map")
		
		# create inputs
		self.toggle_label = tk.Label(in_container, text="Point Cloud Tile Params:")
		self.toggle_label.grid(row=0, column=0, columnspan=2, sticky="nsew")
		
		self.tile_input_var = tk.StringVar(value="250")
		self.tile_size_label = tk.Label(in_container, text="Tile Size (m):")
		self.tile_size_label.grid(row=3, column=0)
		self.tile_size_entry = tk.Entry(in_container, textvariable=self.tile_input_var)
		self.tile_size_entry.grid(row=3, column=1)
		
		self.buff_input_var = tk.StringVar(value="0")
		self.tile_buffer_label = tk.Label(in_container, text="Tile Buffer (m):")
		self.tile_buffer_label.grid(row=4, column=0)
		self.tile_buffer_entry = tk.Entry(in_container, textvariable=self.buff_input_var)
		self.tile_buffer_entry.grid(row=4, column=1)
		
		# create options for tables
		# TODO: options for outputting 2 column table for manual labelling, etc
		
		# create run/ stop button
		self.run_button = tk.Button(btn_container, text="Run", command=self.run_segmentation)
		self.run_button.grid(row=0, column=1)
		self.stop_button = tk.Button(btn_container, text="Stop", command=self.stop_r_thread)
		self.stop_button.grid(row=0, column=2)
		
		# create output text box
		self.toggle_button = tk.Button(text_container, text="Show Output", command=self.toggle_textbox)
		self.toggle_button.grid(row=0, column=0)
		self.textbox = tk.Text(text_container, height=10)
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
	
	def update_textbox(self, text):
		self.textbox.insert(tk.END, text)
	
	def stop_r_thread(self):
		if self.r_thread is not None:
			self.r_thread.stop_flag.set()
			self.run_button.config(state="normal")
	
	def r_stopped(self, text):
		self.run_button.config(state="normal")
		self.textbox.insert(tk.END, text)
	
	def run_segmentation(self):
		self.run_button.config(state="disabled")
		self.textbox.delete("1.0", tk.END)
		self.textbox.grid()
		self.textbox.insert(tk.END, "Commencing Processing")
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
			# Set the command line arguments for the R script
			self.r_script = os.path.join(self.root_path, 'tree_segmentation_v2.R')
			# r_script = "tree_segmentation_v2.R"
			
			# Make the R script executable (equivalent to running "chmod +x r_script" in terminal)
			os.chmod(self.r_script, 0o755)
			
			cmd = ["Rscript", self.r_script]
			args = [str(ortho_path), str(pc_path), str(sd_path), str(self.root_path),
			        str(tile_size), str(tile_buffer), str(cross), str(dtm), str(n_normalized),
			        str(normalized), str(rgb), str(canopy), str(segments), str(overlay)]
			
			# Create RThread and start it
			print(self.r_script)
			r_thread = RThread(self.r_script, args)
			r_thread.start()


class RThread(threading.Thread):
    def __init__(self, r_script, args):
        threading.Thread.__init__(self)
        self.r_script = r_script
        self.args = args

    def run(self):
        process = subprocess.Popen(["Rscript", self.r_script] + self.args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = process.communicate()
        if err:
            print("Error: ", err)
        else:
            print("Output: ", out)



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
		label = tk.Label(self.tw, text=self.text, justify="left", relief="solid", borderwidth=1)
		label.pack(ipadx=1)
	
	def hide(self, event=None):
		if self.tw:
			self.tw.destroy()


if __name__ == "__main__":
	root = tk.Tk()
	root.title("Aurora UAV: Forest Metrics")
	
	app = MainWindow(master=root)
	app.mainloop()



